package hcd

import hcd.algorithms.fullcombinatoric.Algorithm._
import hcd.inout.CmdLineParser.parser
import hcd.inout.DefaultInputConfig
import hcd.inout.InputCsvConversion.{readHcdStudentTopicSelection, readHcdWorkshopPlanning}
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
          distributionAlgorithm(topics, workshops)(studentsSelectedTopics)
        }

      case _ =>
        println("failure") // arguments are bad, error message will have been displayed, nothing more to do
    }

}
