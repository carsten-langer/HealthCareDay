package hcd

import hcd.algorithms._

import java.io.File
import scala.concurrent.duration.Duration

package object inout {

  val defaultCmdLineConfig: CmdLineConfig = CmdLineConfig(
    algorithm = Algorithm.RandomRoundRobin,
    searchDuration = Duration("60s"),
    initialSeed = 0L,

    // workshop planning CSV file
    wFile = new File(""),
    wDelimiter = ';',
    wRowsToSkip = 5,
    wNoTopics = 53,
    wColTopicId = 1, // 1-based A = 1
    wColTopicName = 3,
    wColCategory = 2,
    wColGrades1 = 16,
    wColSeats1 = 17,
    wColGrades2 = 18,
    wColSeats2 = 19,
    wColGrades3 = 20,
    wColSeats3 = 21,
    wFullDayTopics = Seq(9, 45, 46),

    // student topic selection CSV file
    sFile = new File(""),
    sDelimiter = ';',
    sRowsToSkip = 27,
    sNoStudents = 570,
    sColStudentId = 29,
    sColStudentName = 30,
    sColGrade = 28,
    sColFirstSelection = 22,

    // workshop assignments CSV file
    oDelimiter = ';',
  )

}
