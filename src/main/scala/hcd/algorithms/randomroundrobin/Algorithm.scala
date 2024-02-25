package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

import scala.annotation.tailrec

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (_: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) => {

      final case class Student(studentId: StudentId)

      val students = studentsSelectedTopics.toList.map { case (studentId, _) => Student(studentId) }

      @tailrec
      def recursion(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("end of recursion")
            Some(workshopAssignments)
          case ::(Student(studentId), nextStudents) =>
            val updatedWorkshopAssignments = workshopAssignments
              .updatedWith(WorkshopId(0))(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
            recursion(updatedWorkshopAssignments, nextStudents)
        }

      val maybeWorkshopAssignments = recursion(workshopAssignments = Map.empty, remainingStudents = students)
      logger.debug(s"maybeWorkshopAssignments: $maybeWorkshopAssignments")

      // fill in empty assignments for workshop which were not assigned before
      maybeWorkshopAssignments.map(workshopAssignments =>
        workshops.map { case (workshopId, _) => workshopId -> workshopAssignments.getOrElse(workshopId, Set.empty) }
      )

    }

}
