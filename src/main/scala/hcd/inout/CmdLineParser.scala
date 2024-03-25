package hcd.inout

import hcd.BuildInfo
import hcd.algorithms.Algorithm
import scopt.OParser

import java.io.File
import scala.concurrent.duration.Duration

object CmdLineParser {

  private val builder = OParser.builder[CmdLineConfig]

  val parser: OParser[Unit, CmdLineConfig] = {
    import builder._
    //val nl = sys.props("line.separator")
    val d = defaultCmdLineConfig

    OParser.sequence(
      head(BuildInfo.name, BuildInfo.version),
      programName("healthcareday"),
      note("The program reads in a HealthCareDay workshop planning CSV export file."),
      note("General options:"),
      help("help").text("Prints this usage text"),
      version("version").text("Prints the program version"), // actually displays the header text

      note("Options for the distribution algorithm:"),
      opt[Algorithm]("algorithm")
        .valueName(s"<${Algorithm.values.map(_.entryName).mkString("|")}>")
        .action((x, c) => c.copy(algorithm = x))
        .text(s"The distribution algorithm, default: ${d.algorithm.entryName}"),
      opt[Duration]("searchDuration")
        .valueName("<duration>")
        .action((x, c) => c.copy(searchDuration = x))
        .text(s"The duration how long the algorithm shall search for a distribution, e.g. 3600s, 60m, 1h, default: ${d.searchDuration.toSeconds}s"),
      opt[Long]("initialSeed")
        .valueName("<initialSeed>")
        .action((x, c) => c.copy(initialSeed = x))
        .text(s"The initial seed for randomness, default: ${d.initialSeed}"),

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
      opt[Int]("wColTopicName")
        .valueName("<int>")
        .action((x, c) => c.copy(wColTopicName = x))
        .text(s"The column of the topic name (1-based, A = 1, B = 2, ...), default: ${d.wColTopicName}"),
      opt[Int]("wColCategory")
        .valueName("<int>")
        .action((x, c) => c.copy(wColCategory = x))
        .text(s"The column of the topic category (1-based, A = 1, B = 2, ...), default: ${d.wColCategory}"),
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
      opt[Seq[Int]]("wFullDayTopics")
        .valueName("<int,int,...>")
        .action((x, c) => c.copy(wFullDayTopics = x))
        .text(s"The topic ids which represent full day topics, which should be excluded from the distribution, default: ${d.wFullDayTopics.mkString(",")}"),
      arg[File]("<hcd-workshop-planning-file.csv>")
        .required()
        .action((x, c) => c.copy(wFile = x))
        .text("The HealthCareDay workshop planning CSV export file, required"),

      note("Options for reading in the HCD student topic selection CSV file:"),
      opt[Char]("sDelimiter")
        .valueName("<char>")
        .action((x, c) => c.copy(sDelimiter = x))
        .text(s"The column separating character, default: ${d.sDelimiter}"),
      opt[Int]("sRowsToSkip")
        .valueName("<int>")
        .action((x, c) => c.copy(sRowsToSkip = x))
        .text(s"How many rows to skip before the student topic selections start, default: ${d.sRowsToSkip}"),
      opt[Int]("sNoStudents")
        .valueName("<int>")
        .action((x, c) => c.copy(sNoStudents = x))
        .text(s"How many students exist, default: ${d.sNoStudents}"),
      opt[Int]("sColStudentId")
        .valueName("<int>")
        .action((x, c) => c.copy(sColStudentId = x))
        .text(s"The column of the student id (1-based, A = 1, B = 2, ...), default: ${d.sColStudentId}"),
      opt[Int]("sColStudentName")
        .valueName("<int>")
        .action((x, c) => c.copy(sColStudentName = x))
        .text(s"The column of the student name (1-based, A = 1, B = 2, ...), default: ${d.sColStudentName}"),
      opt[Int]("sColGrade")
        .valueName("<int>")
        .action((x, c) => c.copy(sColGrade = x))
        .text(s"The column of the student's grade (1-based, A = 1, B = 2, ...), default: ${d.sColGrade}"),
      opt[Int]("sColFirstSelection")
        .valueName("<int>")
        .action((x, c) => c.copy(sColFirstSelection = x))
        .text(s"The column of the first workshop selection, the other 5 columns are expected to follow (1-based, A = 1, B = 2, ...), default: ${d.sColFirstSelection}"),
      arg[File]("<hcd-student-topic-selection-file.csv>")
        .required()
        .action((x, c) => c.copy(sFile = x))
        .text("The HealthCareDay student topic selection CSV export file, required"),

      note("Options for writing out the HCD distribution CSV files:"),
      opt[Char]("oDelimiter")
        .valueName("<char>")
        .action((x, c) => c.copy(oDelimiter = x))
        .text(s"The column separating character, default: ${d.oDelimiter}"),
    )
  }

}
