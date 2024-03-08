package hcd.algorithms

import com.typesafe.scalalogging.StrictLogging
import hcd.model.Metric.metricGlobal
import hcd.model.Verification.withInputVerification
import hcd.model._
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait AlgorithmBaseSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with StrictLogging {

  /** If property DistributeStudentsToWorkshops is true, run the algorithm for the full model. */
  def maybeRunDistributionAlgorithm(f: FixtureFullDataModel, distributionAlgorithm: DistributionAlgorithm): Unit = {
    // verify input (but not result) and print distributeStudentsToWorkshops for full model
    if (System.getProperty("DistributeStudentsToWorkshops", "false").toBooleanOption.getOrElse(false)) {
      withInputVerification(distributionAlgorithm)(f.topics, f.workshops)(f.studentsSelectedTopics) match {
        case Some(workshopAssignments) =>
          val aPosterioriMetric = metricGlobal(f.topics, f.workshops, f.studentsSelectedTopics)(workshopAssignments)
          logger.info((aPosterioriMetric, workshopAssignments).toString)
        case None => logger.error("Distribution failed!")
      }
    }
  }

}
