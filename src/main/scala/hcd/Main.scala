package hcd

import hcd.inout.CmdLineParser.parser
import hcd.inout.DefaultInputConfig
import hcd.inout.InputCsvConversion.{readHcdStudentTopicSelection, readHcdWorkshopPlanning}
import hcd.model.Metric.metricGlobal
import hcd.model.Verification.withInputVerification
import scopt.OParser

object Main {
  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, DefaultInputConfig) match {
      case Some(config) =>
        println("success")
        val _ = for {
          (topics, workshops) <- readHcdWorkshopPlanning(config)
          studentsSelectedTopics <- readHcdStudentTopicSelection(config)
        } yield {
          //println(topics)
          //println(workshops)
          //println(studentsSelectedTopics)
          val algorithm = withInputVerification(config.algorithm.distributionAlgorithm)
          val maybeWorkshopAssignments = algorithm(topics, workshops)(studentsSelectedTopics)
          println(s"maybeWorkshopAssignments: $maybeWorkshopAssignments")
          val maybeMetric = maybeWorkshopAssignments.map(metricGlobal(topics, workshops, studentsSelectedTopics))
          println(s"maybeMetric: $maybeMetric")
        }

      case _ =>
        println("failure") // arguments are bad, error message will have been displayed, nothing more to do
    }

}
