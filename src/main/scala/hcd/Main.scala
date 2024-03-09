package hcd

import hcd.inout.CmdLineParser.parser
import hcd.inout.DefaultInputConfig
import hcd.inout.InputCsvConversion.{readHcdStudentTopicSelection, readHcdWorkshopPlanning}
import hcd.inout.OutputCsvConversion.{metricCsvFile, studentAssignmentsCsvFile, workshopAssignmentsCsvFile, writeDistribution}
import hcd.model.Verification.withVerification
import hcd.model.{studentsSelectedTopicsFrom, topicsFrom}
import scopt.OParser

object Main {
  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, DefaultInputConfig) match {
      case Some(config) =>
        val _ = for {
          (topicsWithName, workshops) <- readHcdWorkshopPlanning(config)
          studentsSelectedTopicsWithName <- readHcdStudentTopicSelection(config)
        } yield {
          val algorithm = withVerification(config.algorithm.distributionAlgorithm)
          val topics = topicsFrom(topicsWithName)
          val studentsSelectedTopics = studentsSelectedTopicsFrom(studentsSelectedTopicsWithName)
          algorithm(topics, workshops)(studentsSelectedTopics) match {
            case None => println("No distribution of students to workshops found!")
            case Some(workshopAssignments) =>
              println("Distribution of students to workshops found!")
              writeDistribution(config)(topicsWithName, workshops, studentsSelectedTopicsWithName)(workshopAssignments)
              println(s"Metric written to file $metricCsvFile")
              println(s"Workshop assignments written to file $workshopAssignmentsCsvFile")
              println(s"Student assignments written to file $studentAssignmentsCsvFile")
          }
        }

      case _ => () // arguments are bad, error message will have been displayed, nothing more to do
    }

}
