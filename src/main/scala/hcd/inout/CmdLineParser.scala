package hcd.inout

import hcd.BuildInfo
import hcd.algorithms.Algorithm
import hcd.model.{DistributeWorkshopFilling, Seed}
import scopt.OParser

import java.io.File
import scala.concurrent.duration.Duration

object CmdLineParser {

  private val builder = OParser.builder[CmdLineConfig]

  val parser: OParser[Unit, CmdLineConfig] = {
    import builder._
    //val nl = sys.props("line.separator")
    val d = CmdLineConfig.default

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
      opt[Seed]("initialSeed")
        .valueName("<long>")
        .action((x, c) => c.copy(initialSeed = x))
        .text(s"The initial seed for randomness, default: ${d.initialSeed}"),
      opt[DistributeWorkshopFilling]("distributeWorkshopFilling")
        .valueName("<boolean>")
        .action((x, c) => c.copy(distributeWorkshopFilling = x))
        .text(s"Flag (true/false) if the workshop filling shall be more distributed, default: ${d.distributeWorkshopFilling}"),

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
      opt[Int]("wColPreassignedTopic")
        .valueName("<int>")
        .action((x, c) => c.copy(wColPreassignedTopic = x))
        .text(s"The column of the flag if the topic is pre-assigned (1-based, A = 1, B = 2, ...), default: ${d.wColPreassignedTopic}"),
      opt[Int]("wColOnlyVoluntaryTopic")
        .valueName("<int>")
        .action((x, c) => c.copy(wColOnlyVoluntaryTopic = x))
        .text(s"The column of the flag if the topic is only assignable if the student selected it (1-based, A = 1, B = 2, ...), default: ${d.wColOnlyVoluntaryTopic}"),
      opt[Int]("wColSexes1")
        .valueName("<int>")
        .action((x, c) => c.copy(wColSexes1 = x))
        .text(s"The column of the set of sexes for the first topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColSexes1}"),
      opt[Int]("wColGrades1")
        .valueName("<int>")
        .action((x, c) => c.copy(wColGrades1 = x))
        .text(s"The column of the set of grades for the first topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColGrades1}"),
      opt[Int]("wColMinSeats1")
        .valueName("<int>")
        .action((x, c) => c.copy(wColMinSeats1 = x))
        .text(s"The column of the minimum number of seats for the first topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColMinSeats1}"),
      opt[Int]("wColMaxSeats1")
        .valueName("<int>")
        .action((x, c) => c.copy(wColMaxSeats1 = x))
        .text(s"The column of the maximum number of seats for the first topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColMaxSeats1}"),
      opt[Int]("wColSexes2")
        .valueName("<int>")
        .action((x, c) => c.copy(wColSexes2 = x))
        .text(s"The column of the set of sexes for the second topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColSexes2}"),
      opt[Int]("wColGrades2")
        .valueName("<int>")
        .action((x, c) => c.copy(wColGrades2 = x))
        .text(s"The column of the set of grades for the second topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColGrades2}"),
      opt[Int]("wColMinSeats2")
        .valueName("<int>")
        .action((x, c) => c.copy(wColMinSeats2 = x))
        .text(s"The column of the minimum number of seats for the second topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColMinSeats2}"),
      opt[Int]("wColMaxSeats2")
        .valueName("<int>")
        .action((x, c) => c.copy(wColMaxSeats2 = x))
        .text(s"The column of the maximum number of seats for the second topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColMaxSeats2}"),
      opt[Int]("wColSexes3")
        .valueName("<int>")
        .action((x, c) => c.copy(wColSexes3 = x))
        .text(s"The column of the set of sexes for the third topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColSexes3}"),
      opt[Int]("wColGrades3")
        .valueName("<int>")
        .action((x, c) => c.copy(wColGrades3 = x))
        .text(s"The column of the set of grades for the third topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColGrades3}"),
      opt[Int]("wColMinSeats3")
        .valueName("<int>")
        .action((x, c) => c.copy(wColMinSeats3 = x))
        .text(s"The column of the minimum number of seats for the third topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColMinSeats3}"),
      opt[Int]("wColMaxSeats3")
        .valueName("<int>")
        .action((x, c) => c.copy(wColMaxSeats3 = x))
        .text(s"The column of the maximum number of seats for the third topic timeslot (1-based, A = 1, B = 2, ...), default: ${d.wColMaxSeats3}"),
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
      opt[Int]("sColSex")
        .valueName("<int>")
        .action((x, c) => c.copy(sColSex = x))
        .text(s"The column of the student's sex (1-based, A = 1, B = 2, ...), default: ${d.sColSex}"),
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
