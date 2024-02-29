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
                              )

      // ordering of workshops is necessary for the unit tests to know the expected result
      val orderedWorkshops = workshops.toList.sortBy { case (workshopId, _) => workshopId.id }

      val allTimeSlots = Set[TimeSlot](FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)

      // ordering the topic selections per student is necessary for the unit tests to know the expected result
      val students = studentsSelectedTopics.toList.map {
        case (studentId, (_, selectedTopics)) =>
          val topicSelections = selectedTopics.toList.map(_.swap).map(TopicSelection.tupled)
          val orderedTopicSelection = topicSelections.sortBy(_.selectionPriority.prio)
          Student(studentId, orderedTopicSelection, allTimeSlots)
      }

      // See https://github.com/scala/bug/issues/6675 and https://github.com/scala/bug/issues/6111
      // for the need for a holder to avoid deprecation message on (scala/bug#6675)
      case class Holder[T](_1: T) extends Product1[T]

      def findWorkshopId(student: Student): Option[(WorkshopId, TopicId, TimeSlot)] = {
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

      @tailrec
      def recursion(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("Successful end of recursion.")
            Some(workshopAssignments)
          case ::(headStudent@Student(studentId, topicSelections, unassignedTimeSlots), nextStudents) =>
            findWorkshopId(headStudent) match {
              case None =>
                // skip this student as no workshops could be found now, the student will get assigned workshops from the left-overs
                recursion(workshopAssignments, nextStudents)
              case Some((foundWorkshopId, foundTopicId, foundTimeSlot)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedTopicSelections = topicSelections.filterNot(_.topicId == foundTopicId)
                val updatedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudent = headStudent.copy(
                  topicSelections = updatedTopicSelections,
                  unassignedTimeSlots = updatedTimeSlots,
                )
                val updatedStudents = updatedStudent :: nextStudents
                recursion(updatedWorkshopAssignments, updatedStudents)
            }
        }

      val maybeWorkshopAssignments = recursion(workshopAssignments = Map.empty, remainingStudents = students)
      logger.debug(s"maybeWorkshopAssignments: $maybeWorkshopAssignments")

      // fill in empty assignments for workshop which were not assigned before
      maybeWorkshopAssignments.map { workshopAssignments =>
        val unassignedStudents = students.filterNot(student => workshopAssignments.exists {
          case (_, studentIds) => studentIds.contains(student.studentId)
        }).map(_.studentId)
        // assign all unassigned students to workshop 0
        val workshopAssignmentsInclUnassignedStudents = workshopAssignments
          .updatedWith(WorkshopId(0))(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) ++ unassignedStudents))
        workshops.map { case (workshopId, _) =>
          workshopId -> workshopAssignmentsInclUnassignedStudents.getOrElse(workshopId, Set.empty)
        }
      }

    }

}
