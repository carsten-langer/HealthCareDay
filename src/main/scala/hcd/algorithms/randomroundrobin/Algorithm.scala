package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

import scala.annotation.tailrec

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (_: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) => {

      final case class Student()

      val students = studentsSelectedTopics.toList.map(_ => Student())

      @tailrec
      def recursion(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("end of recursion")
            Some(workshopAssignments)
          case _ =>
            val updatedWorkshopAssignments = workshopAssignments.updated(WorkshopId(0), Set(StudentId(1)))
            recursion(updatedWorkshopAssignments, List.empty)
        }

      val emptyWorkshopAssignments = workshops.view.mapValues(_ => Set.empty[StudentId]).toMap

      val maybeWorkshopAssignments = recursion(emptyWorkshopAssignments, remainingStudents = students)
      logger.debug(s"maybeWorkshopAssignments: $maybeWorkshopAssignments")
      maybeWorkshopAssignments

    }

}
