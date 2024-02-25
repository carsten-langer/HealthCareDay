package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (_: Topics, _: Workshops) => (_: StudentsSelectedTopics) => {

      final case class Student()

      def recursion(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("end of recursion")
            Some(workshopAssignments)
          case _ => ???
        }

      val maybeWorkshopAssignments = recursion(workshopAssignments = Map.empty, remainingStudents = List.empty)
      logger.debug(s"maybeWorkshopAssignments: $maybeWorkshopAssignments")
      maybeWorkshopAssignments

    }

}
