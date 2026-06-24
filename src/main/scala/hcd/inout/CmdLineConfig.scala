package hcd.inout

import hcd.algorithms.Algorithm
import hcd.model.{Flag, Seed}

import java.io.File
import scala.concurrent.duration.Duration

case class CmdLineConfig(
                          algorithm: Algorithm,
                          searchDuration: Duration,
                          initialSeed: Seed,
                          distributeWorkshopFilling: Flag,

                          // workshop planning CSV file
                          wFile: File,
                          wDelimiter: Char,
                          wRowsToSkip: Int,
                          wNoTopics: Int,
                          wColTopicId: Int, // 1-based A = 1
                          wColTopicName: Int,
                          wColCategory: Int,
                          wColPreassignedTopic: Int,
                          wColOnlyVoluntaryTopic: Int,
                          wColGrades1: Int,
                          wColSeats1: Int,
                          wColGrades2: Int,
                          wColSeats2: Int,
                          wColGrades3: Int,
                          wColSeats3: Int,

                          // student topic selection CSV file
                          sFile: File,
                          sDelimiter: Char,
                          sRowsToSkip: Int,
                          sNoStudents: Int,
                          sColStudentId: Int,
                          sColStudentName: Int,
                          sColGrade: Int,
                          sColFirstSelection: Int,

                          // workshop assignments CSV file
                          oDelimiter: Char,
                        )

object CmdLineConfig {

  val default: CmdLineConfig = CmdLineConfig(
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