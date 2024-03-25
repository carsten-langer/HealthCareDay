package hcd

import hcd.inout.CmdLineParser.parser
import hcd.inout.InputCsvConversion.{readHcdStudentTopicSelection, readHcdWorkshopPlanning}
import hcd.inout.OutputCsvConversion._
import hcd.inout.defaultCmdLineConfig
import hcd.model.Verification.withVerification
import hcd.model.{ShallStop, studentsSelectedTopicsFrom, topicsFrom}
import scopt.OParser

import java.time.LocalDateTime

object Main {
  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, defaultCmdLineConfig) match {
      case Some(config) =>
        val _ = for {
          (topicsWithName, workshops) <- readHcdWorkshopPlanning(config)
          studentsSelectedTopicsWithName <- readHcdStudentTopicSelection(config)
        } yield {
          initWriteDistribution(config)
          val saveIntermediateState = writeDistribution(config)(topicsWithName, workshops, studentsSelectedTopicsWithName)
          val startDateTime = LocalDateTime.now()
          val searchLimit = startDateTime.plusSeconds(config.searchDuration.toSeconds)

          def shallStop: ShallStop = () => LocalDateTime.now().isAfter(searchLimit)

          val algorithm = withVerification(config.algorithm.distributionAlgorithm(saveIntermediateState)(shallStop))
          val topics = topicsFrom(topicsWithName)
          val studentsSelectedTopics = studentsSelectedTopicsFrom(studentsSelectedTopicsWithName)
          algorithm(topics, workshops)(studentsSelectedTopics) match {
            case None => println("No distribution of students to workshops found!")
            case Some(workshopAssignments) =>
              println("Distribution of students to workshops found!")
              saveIntermediateState(workshopAssignments)
              println(s"Metric written to file $metricCsvFile")
              println(s"Workshop assignments written to file $workshopAssignmentsCsvFile")
              println(s"Student assignments written to file $studentAssignmentsCsvFile")
          }
        }

      case _ => () // arguments are bad, error message will have been displayed, nothing more to do
    }

}
