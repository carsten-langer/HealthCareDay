package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

import scala.annotation.tailrec

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (_: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) => {

      // Ordering the SelectedTopics per student is necessary for the unit tests to know the expected result, thus
      // we need a new data type. Ordering makes most sense by selection priority, thus we use the flipped order of
      // values compared to SelectedTopics
      final case class TopicSelection(selectionPriority: SelectionPriority, topicId: TopicId)

      final case class Student(
                                studentId: StudentId,
                                topicSelections: List[TopicSelection],
                                unassignedTimeSlots: Set[TimeSlot],
                                assignedTopics: Set[TopicId],
                              )

      // ordering of workshops is necessary for the unit tests to know the expected result
      val orderedWorkshops = workshops.toList.sortBy { case (workshopId, _) => workshopId.id }

      val allTimeSlots = Set[TimeSlot](FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)

      // ordering the topic selections per student is necessary for the unit tests to know the expected result
      val students = studentsSelectedTopics.toList.map {
        case (studentId, (_, selectedTopics)) =>
          val topicSelections = selectedTopics.toList.map(_.swap).map(TopicSelection.tupled)
          val orderedTopicSelection = topicSelections.sortBy(_.selectionPriority.prio)
          Student(studentId, orderedTopicSelection, allTimeSlots, assignedTopics = Set.empty)
      }

      // See https://github.com/scala/bug/issues/6675 and https://github.com/scala/bug/issues/6111
      // for the need for a holder to avoid deprecation message on (scala/bug#6675)
      case class Holder[T](_1: T) extends Product1[T]

      // First round of distribution: For a student, select the next workshop being part of her selection and which
      // otherwise fulfils all criteria.
      def findWorkshopId1(student: Student): Option[(WorkshopId, TopicId, TimeSlot)] = {
        object ExtractorWorkshopForTopic {
          def unapply(topicSelection: TopicSelection): Option[Holder[(WorkshopId, TopicId, TimeSlot)]] =
            orderedWorkshops.collectFirst {
              case (workshopId, (topicId, timeSlot, _, _))
                if topicId == topicSelection.topicId &&
                  student.unassignedTimeSlots.contains(timeSlot) =>
                Holder((workshopId, topicId, timeSlot))
            }
        }

        student.topicSelections.collectFirst {
          case ExtractorWorkshopForTopic(Holder((workshopId, topicId, timeSlot))) => (workshopId, topicId, timeSlot)
        }
      }

      // First round of distribution: For each student, select the next workshop being part of her selection and which
      // otherwise fulfils all criteria.
      @tailrec
      def recursion1(
                      workshopAssignments: WorkshopAssignments,
                      undistributableStudents: List[Student],
                      studentsToDistribute: List[Student],
                    ): Option[(WorkshopAssignments, List[Student])] =
        studentsToDistribute match {
          case Nil =>
            logger.debug("Successful end of recursion1.")
            Some((workshopAssignments, undistributableStudents))
          case ::(headStudent@Student(studentId, topicSelections, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId1(headStudent) match {
              case None =>
                // skip this student as no workshops could be found now, the student will get assigned workshops from 2nd round.
                val updatedUndistributableStudents = undistributableStudents :+ headStudent
                recursion1(workshopAssignments, updatedUndistributableStudents, nextStudents)
              case Some((foundWorkshopId, foundTopicId, foundTimeSlot)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val updatedTopicSelections = topicSelections.filterNot(_.topicId == foundTopicId)
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      topicSelections = updatedTopicSelections,
                      unassignedTimeSlots = updatedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    updatedStudent :: nextStudents
                  }
                recursion1(updatedWorkshopAssignments, undistributableStudents, updatedStudents)
            }
        }

      val maybeDistribution1 = recursion1(workshopAssignments = Map.empty, undistributableStudents = List.empty, studentsToDistribute = students)
      logger.debug(s"maybeDistribution1: $maybeDistribution1")

      // Second round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection.
      def findWorkshopId2(student: Student): Option[(WorkshopId, TopicId, TimeSlot)] = orderedWorkshops.collectFirst {
        case (workshopId, (topicId, timeSlot, _, _))
          if student.unassignedTimeSlots.contains(timeSlot) &&
            !student.assignedTopics.contains(topicId) =>
          logger.trace(s"found $workshopId at $timeSlot for $student.")
          (workshopId, topicId, timeSlot)
      }

      // Second round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection.
      @tailrec
      def recursion2(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("Successful end of recursion2.")
            Some(workshopAssignments)
          case ::(headStudent@Student(studentId, _, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId2(headStudent) match {
              case None =>
                logger.debug(s"Unsuccessful end of recursion2. No suitable workshop found for student $headStudent.")
                None
              case Some((foundWorkshopId, foundTopicId, foundTimeSlot)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      unassignedTimeSlots = updatedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    updatedStudent :: nextStudents
                  }
                recursion2(updatedWorkshopAssignments, updatedStudents)
            }
        }

      val maybeWorkshopAssignments2 = maybeDistribution1.flatMap((recursion2 _).tupled)
      logger.debug(s"maybeWorkshopAssignments2: $maybeWorkshopAssignments2")

      // make sure each workshop has a set of students. If not yet the case, add an empty set.
      maybeWorkshopAssignments2.map(workshopAssignments =>
        workshops.map { case (workshopId, _) =>
          workshopId -> workshopAssignments.getOrElse(workshopId, Set.empty)
        }
      )

    }

}
