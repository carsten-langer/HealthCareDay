package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (_: Topics, workshops: Workshops) => (_: StudentsSelectedTopics) => {

      final case class Student()

      def recursion(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("end of recursion")
            Some(workshopAssignments)
          case _ => ???
        }

      val emptyWorkshopAssignments = workshops.view.mapValues(_ => Set.empty[StudentId]).toMap

      val maybeWorkshopAssignments = recursion(emptyWorkshopAssignments, remainingStudents = List.empty)
      logger.debug(s"maybeWorkshopAssignments: $maybeWorkshopAssignments")
      maybeWorkshopAssignments

    }

}
