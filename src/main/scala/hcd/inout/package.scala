package hcd

import hcd.algorithms._

import java.io.File
import scala.concurrent.duration.Duration

package object inout {

  val defaultCmdLineConfig: CmdLineConfig = CmdLineConfig(
    algorithm = Algorithm.RandomRoundRobin,
    searchDuration = Duration("60s"),
    initialSeed = 0L,
    distributeWorkshopFilling = false,

    // workshop planning CSV file
    wFile = new File(""),
    wDelimiter = ';',
    wRowsToSkip = 4,
    wNoTopics = 62,
    wColTopicId = 1, // 1-based A = 1
    wColTopicName = 3,
    wColCategory = 2,
    wColPreassignedTopic = 14,
    wColOnlyVoluntaryTopic = 15,
    wColGrades1 = 16,
    wColSeats1 = 17,
    wColGrades2 = 18,
    wColSeats2 = 19,
    wColGrades3 = 20,
    wColSeats3 = 21,

    // student topic selection CSV file
    sFile = new File(""),
    sDelimiter = ';',
    sRowsToSkip = 1,
    sNoStudents = 533,
    sColStudentId = 2,
    sColStudentName = 4,
    sColGrade = 3,
    sColFirstSelection = 5,

    // workshop assignments CSV file
    oDelimiter = ';',
  )

}
