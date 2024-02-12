package hcd

import hcd.inout.CmdLineParser.parser
import hcd.inout.DefaultInputConfig
import hcd.inout.InputCsvConversion.readHcdWorkshopPlanning
import scopt.OParser

object Main {
  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, DefaultInputConfig) match {
      case Some(config) =>
        println("success")
        println(BuildInfo)
        println(config)
        readHcdWorkshopPlanning(config)
      case _ => println("failure") // arguments are bad, error message will have been displayed, nothing more to do
    }

}
