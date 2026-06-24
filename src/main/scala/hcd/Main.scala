package hcd

import hcd.inout.InputCsvConversion.{readHcdStudentTopicSelection, readHcdWorkshopPlanning}
import hcd.inout.OutputCsvConversion._
import hcd.inout.{CmdLineConfig, CmdLineParser}
import hcd.model.Verification.withVerification
import hcd.model.{ShallStop, studentsSelectedTopicsFrom}
import scopt.OParser

import java.time.LocalDateTime

object Main {
  def main(args: Array[String]): Unit =
    OParser.parse(CmdLineParser.parser, args, CmdLineConfig.default) match {
      case Some(config) =>
        val unitT = for {
          (topics, workshops) <- readHcdWorkshopPlanning(config)
          studentsNameSelectedTopics <- readHcdStudentTopicSelection(config)
        } yield {
          initWriteDistribution(config)
          val saveIntermediateState = writeDistribution(config)(topics, workshops, studentsNameSelectedTopics)
          val startDateTime = LocalDateTime.now()
          val searchLimit = startDateTime.plusSeconds(config.searchDuration.toSeconds)

          def shallStop: ShallStop = () => LocalDateTime.now().isAfter(searchLimit)

          val distributionAlgorithm = config.algorithm.distributionAlgorithm
          val initialSeed = config.initialSeed
          val distributeWorkshopFilling = config.distributeWorkshopFilling
          val configuredAlgorithm = distributionAlgorithm(initialSeed)(saveIntermediateState)(shallStop)
          val algorithm = withVerification(configuredAlgorithm)
          val studentsSelectedTopics = studentsSelectedTopicsFrom(studentsNameSelectedTopics)
          algorithm(distributeWorkshopFilling, topics, workshops, studentsSelectedTopics) match {
            case None => println("No distribution of students to workshops found!")
            case Some(workshopAssignments) =>
              println("Distribution of students to workshops found!")
              saveIntermediateState(workshopAssignments)
              println(s"Metric written to file $metricCsvFile")
              println(s"Workshop assignments written to file $workshopAssignmentsCsvFile")
              println(s"Student assignments written to file $studentAssignmentsCsvFile")
          }
        }
        unitT.getOrElse(unitT.failed.foreach { t =>
          println("Distribution failed with:")
          t.printStackTrace()
        })

      case _ => () // arguments are bad, error message will have been displayed, nothing more to do
    }

}
