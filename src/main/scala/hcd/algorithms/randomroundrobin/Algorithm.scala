package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

import scala.annotation.tailrec

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (topics: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) => {

      // Ordering the SelectedTopics per student is necessary for the unit tests to know the expected result, thus
      // we need a new data type. Ordering makes most sense by selection priority, thus we use the flipped order of
      // values compared to SelectedTopics
      final case class TopicSelection(selectionPriority: SelectionPriority, topicId: TopicId)

      final case class Student(
                                algoPrio: Int,
                                studentId: StudentId,
                                grade: Grade,
                                topicSelections: List[TopicSelection],
                                unassignedTimeSlots: Set[TimeSlot],
                                assignedTopics: Set[TopicId],
                              )

      // ordering of workshops is necessary for the unit tests to know the expected result
      val orderedWorkshops = workshops.toList.sortBy { case (workshopId, _) => workshopId.id }

      val allTimeSlots = Set[TimeSlot](FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)

      def orderStudents(students: List[Student]): List[Student] = students.sortBy(s => (s.algoPrio, s.studentId.id))

      // ordering both the students and the topic selections per student is necessary for the unit tests
      // to know the expected result
      val orderedStudents = orderStudents(studentsSelectedTopics.toList.map {
        case (studentId, (grade, selectedTopics)) =>
          val topicSelections = selectedTopics.toList.map(_.swap).map(TopicSelection.tupled)
          val orderedTopicSelection = topicSelections.sortBy(_.selectionPriority.prio)
          Student(algoPrio = 1, studentId, grade, orderedTopicSelection, allTimeSlots, assignedTopics = Set.empty)
      })

      def haveMinVaryingCategories(topicCandidates: Set[TopicId]): Boolean =
        topicCandidates.toList.map(topics).count(_ == Nutrition) < 3 &&
          topicCandidates.toList.map(topics).count(_ == Relaxation) < 3

      def haveMaxVaryingCategories(topicCandidates: Set[TopicId]): Boolean =
        haveMinVaryingCategories(topicCandidates) &&
          topicCandidates.toList.map(topics).count(_ == Sports) < 3

      type FindWorkshopId12 = (Student, WorkshopAssignments) => Option[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]
      type FindWorkshopId23 = (Student, WorkshopAssignments) => Option[(WorkshopId, TopicId, TimeSlot)]

      // See https://github.com/scala/bug/issues/6675 and https://github.com/scala/bug/issues/6111
      // for the need for a holder to avoid deprecation message on (scala/bug#6675)
      case class Holder[T](_1: T) extends Product1[T]

      // First round of distribution: For a student, select the next workshop being part of her selection and which
      // otherwise fulfils all criteria.
      def findWorkshopId1: FindWorkshopId12 = (student: Student, workshopAssignments: WorkshopAssignments) => {
        object ExtractorWorkshopForTopic {
          def unapply(topicSelection: TopicSelection): Option[Holder[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]] =
            orderedWorkshops.collectFirst {
              case (workshopId, (topicSelection.topicId, timeSlot, grades, seats))
                if student.unassignedTimeSlots.contains(timeSlot) &&
                  grades.contains(student.grade) &&
                  workshopAssignments.getOrElse(workshopId, Set.empty).size < seats.n &&
                  haveMaxVaryingCategories(student.assignedTopics + topicSelection.topicId) =>
                logger.trace(s"found1: $workshopId at $timeSlot for $student.")
                Holder((workshopId, topicSelection.topicId, topicSelection.selectionPriority, timeSlot))
            }
        }

        student.topicSelections.collectFirst {
          case ExtractorWorkshopForTopic(Holder((workshopId, topicId, selectionPriority, timeSlot))) =>
            (workshopId, topicId, selectionPriority, timeSlot)
        }
      }

      // First and second round of distribution:
      // For each student, select the next workshop which fulfills the criteria given in findWorkshopId function.
      // If no workshop can be found, skip the student and leave the distribution to the next round.
      @tailrec
      def recursion12(findWorkshopId: FindWorkshopId12)(
        workshopAssignments: WorkshopAssignments,
        undistributableStudents: List[Student],
        studentsToDistribute: List[Student],
      ): Option[(WorkshopAssignments, List[Student])] =
        studentsToDistribute match {
          case Nil =>
            logger.debug("Successful end of recursion12.")
            Some((workshopAssignments, undistributableStudents))
          case ::(headStudent@Student(algoPrio, studentId, _, topicSelections, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId(headStudent, workshopAssignments) match {
              case None =>
                // skip this student as no workshops could be found now, the student will get assigned workshops from next round.
                val updatedUndistributableStudents = undistributableStudents :+ headStudent
                recursion12(findWorkshopId)(workshopAssignments, updatedUndistributableStudents, nextStudents)
              case Some((foundWorkshopId, foundTopicId, SelectionPriority(prio), foundTimeSlot)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val (_, updatedTopicSelections) = topicSelections.span(_.selectionPriority.prio <= prio)
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      algoPrio = algoPrio + 6 - prio,
                      topicSelections = updatedTopicSelections,
                      unassignedTimeSlots = updatedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    // While it does not make a difference for the resulting total order if we write
                    // orderStudents(updatedStudent :: nextStudents) or
                    // orderStudents(nextStudents :+ updatedStudent)
                    // the latter expresses better that students just assigned go the back of the list
                    // It may also have a performance improvement if the uses sorting algorithm handles nearly-sorted
                    // lists well.
                    orderStudents(nextStudents :+ updatedStudent)
                  }
                recursion12(findWorkshopId)(updatedWorkshopAssignments, undistributableStudents, updatedStudents)
            }
        }

      val maybeDistribution1 = recursion12(findWorkshopId1)(
        workshopAssignments = Map.empty,
        undistributableStudents = List.empty,
        studentsToDistribute = orderedStudents
      )
      logger.debug(s"maybeDistribution1: $maybeDistribution1")

      // Second or third round of distribution: For each student, select the next workshop which fulfils all mandatory
      // criteria and all criteria by the given function hasVaryingCategories, regardless of the student's selection.
      def findWorkshopId23(haveVaryingCategories: Set[TopicId] => Boolean): FindWorkshopId23 = (student: Student, workshopAssignments: WorkshopAssignments) =>
        orderedWorkshops.collectFirst {
          case (workshopId, (topicId, timeSlot, grades, seats))
            if student.unassignedTimeSlots.contains(timeSlot) &&
              !student.assignedTopics.contains(topicId) &&
              grades.contains(student.grade) &&
              workshopAssignments.getOrElse(workshopId, Set.empty).size < seats.n &&
              haveVaryingCategories(student.assignedTopics + topicId) =>
            logger.trace(s"found23: $workshopId at $timeSlot for $student.")
            (workshopId, topicId, timeSlot)
        }

      // Second round of distribution: adopt findWorkshopId23 to the signature needed in recursion12.
      def findWorkshopId2: FindWorkshopId12 = (student: Student, workshopAssignments: WorkshopAssignments) =>
        findWorkshopId23(haveMaxVaryingCategories)(student, workshopAssignments).map {
          case (workshopId, topicId, timeSlot) =>
            (workshopId, topicId, SelectionPriority(Int.MaxValue), timeSlot)
        }

      // Second round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection.
      val maybeDistribution2 = maybeDistribution1.flatMap { case (workshopAssignments, notYetDistributedStudents) =>
        recursion12(findWorkshopId2)(
          workshopAssignments,
          undistributableStudents = List.empty,
          studentsToDistribute = notYetDistributedStudents
        )
      }
      logger.debug(s"maybeDistribution2: $maybeDistribution2")

      // Third round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection, with the exception of the criteria that no 3 workshops of category sports shall be assigned.
      def findWorkshopId3: FindWorkshopId23 = findWorkshopId23(haveMinVaryingCategories)

      // Third round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection, with the exception of the criteria that no 3 workshops of category sports shall be assigned.
      // If no workshop can be found, the distribution fails.
      @tailrec
      def recursion3(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("Successful end of recursion2.")
            Some(workshopAssignments)
          case ::(headStudent@Student(_, studentId, _, _, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId3(headStudent, workshopAssignments) match {
              case None =>
                logger.debug(s"Unsuccessful end of recursion3. No suitable workshop found for student $headStudent.")
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
                recursion3(updatedWorkshopAssignments, updatedStudents)
            }
        }

      val maybeWorkshopAssignments3 = maybeDistribution2.flatMap((recursion3 _).tupled)
      logger.debug(s"maybeWorkshopAssignments3: $maybeWorkshopAssignments3")

      // make sure each workshop has a set of students. If not yet the case, add an empty set.
      maybeWorkshopAssignments3.map(workshopAssignments =>
        workshops.map { case (workshopId, _) =>
          workshopId -> workshopAssignments.getOrElse(workshopId, Set.empty)
        }
      )

    }

}
