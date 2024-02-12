package hcd.inout

import hcd.BuildInfo
import scopt.OParser

import java.io.File

object CmdLineParser {

  private val builder = OParser.builder[InputConfig]

  val parser: OParser[Unit, InputConfig] = {
    import builder._
    //val nl = sys.props("line.separator")
    val d = DefaultInputConfig

    OParser.sequence(
      head(BuildInfo.name, BuildInfo.version),
      programName("healthcareday"),
      note("The program reads in a HealthCareDay workshop planning CSV export file."),
      note("General options:"),
      help("help").text("Prints this usage text"),
      version("version").text("Prints the program version"), // actually displays the header text
      note("Options for reading in the HCD workshop planning CSV file:"),
      opt[Char]("wDelimiter")
        .valueName("<char>")
        .action((x, c) => c.copy(wDelimiter = x))
        .text(s"The column separating character, default: ${d.wDelimiter}"),
      opt[Int]("wRowsToSkip")
        .valueName("<int>")
        .action((x, c) => c.copy(wRowsToSkip = x))
        .text(s"How many rows to skip before the topics start, default: ${d.wRowsToSkip}"),
      opt[Int]("wNoTopics")
        .valueName("<int>")
        .action((x, c) => c.copy(wNoTopics = x))
        .text(s"How many topics exist, default: ${d.wNoTopics}"),
      opt[Int]("wColTopicId")
        .valueName("<int>")
        .action((x, c) => c.copy(wColTopicId = x))
        .text(s"The column of the topic id (1-based, A = 1, B = 2, ...), default: ${d.wColTopicId}"),
      opt[Int]("wColCategory")
        .valueName("<int>")
        .action((x, c) => c.copy(wColCategory = x))
        .text(s"The column of the topic category (1-based, A = 1, B = 2, ...), default: ${d.wColCategory}"),
      opt[Int]("wColName")
        .valueName("<int>")
        .action((x, c) => c.copy(wColName = x))
        .text(s"The column of the topic name (1-based, A = 1, B = 2, ...), default: ${d.wColName}"),
      opt[Int]("wColGrades1")
        .valueName("<int>")
        .action((x, c) => c.copy(wColGrades1 = x))
        .text(s"The column of the set of grades for the first topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColGrades1}"),
      opt[Int]("wColSeats1")
        .valueName("<int>")
        .action((x, c) => c.copy(wColSeats1 = x))
        .text(s"The column of the number of seats for the first topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColSeats1}"),
      opt[Int]("wColGrades2")
        .valueName("<int>")
        .action((x, c) => c.copy(wColGrades2 = x))
        .text(s"The column of the set of grades for the second topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColGrades2}"),
      opt[Int]("wColSeats2")
        .valueName("<int>")
        .action((x, c) => c.copy(wColSeats2 = x))
        .text(s"The column of the number of seats for the second topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColSeats2}"),
      opt[Int]("wColGrades3")
        .valueName("<int>")
        .action((x, c) => c.copy(wColGrades3 = x))
        .text(s"The column of the set of grades for the third topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColGrades3}"),
      opt[Int]("wColSeats3")
        .valueName("<int>")
        .action((x, c) => c.copy(wColSeats3 = x))
        .text(s"The column of the number of seats for the third topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColSeats3}"),
      arg[File]("<hcd-workshop-planning-file.csv>")
        .required()
        .action((x, c) => c.copy(wFile = x))
        .text("The HealthCareDay workshop planning CSV export file, required"),
    )
  }

}
