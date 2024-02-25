package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model._

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm = (_: Topics, _: Workshops) => (_: StudentsSelectedTopics) =>
    Some(Map.empty)

}
