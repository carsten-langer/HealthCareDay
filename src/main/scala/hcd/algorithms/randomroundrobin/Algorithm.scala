package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

import scala.annotation.tailrec

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (_: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) => {

      final case class Student(studentId: StudentId, selectedTopicIds: Set[TopicId])

      // ordering of workshops is necessary for the unit tests to know the expected result
      val orderedWorkshops = workshops.toList.sortBy { case (workshopId, _) => workshopId.id }

      val students = studentsSelectedTopics.toList.map {
        case (studentId, (_, selectedTopics)) => Student(studentId, selectedTopics.keySet.toSet)
      }

      // See https://github.com/scala/bug/issues/6675 and https://github.com/scala/bug/issues/6111
      // for the need for a holder to avoid deprecation message on (scala/bug#6675)
      case class Holder[T](_1: T) extends Product1[T]

      def findWorkshopId(student: Student): Option[(WorkshopId, TopicId)] = {
        object ExtractorWorkshopForTopic {
          def unapply(topicId: TopicId): Option[Holder[(WorkshopId, TopicId)]] =
            orderedWorkshops.collectFirst {
              case (workshopId, (`topicId`, _, _, _)) => Holder((workshopId, topicId))
            }
        }

        student.selectedTopicIds.collectFirst {
          case ExtractorWorkshopForTopic(Holder((workshopId, topicId))) => (workshopId, topicId)
        }
      }

      @tailrec
      def recursion(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("Successful end of recursion.")
            Some(workshopAssignments)
          case ::(headStudent@Student(studentId, selectedTopicIds), nextStudents) =>
            findWorkshopId(headStudent) match {
              case None =>
                // skip this student as no workshops could be found now, the student will get assigned workshops from the left-overs
                recursion(workshopAssignments, nextStudents)
              case Some((foundWorkshopId, foundTopicId)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedSelectedTopics = selectedTopicIds - foundTopicId
                val updatedStudent = headStudent.copy(selectedTopicIds = updatedSelectedTopics)
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
