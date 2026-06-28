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
                          wColMinSeats1: Int,
                          wColMaxSeats1: Int,
                          wColGrades2: Int,
                          wColMinSeats2: Int,
                          wColMaxSeats2: Int,
                          wColGrades3: Int,
                          wColMinSeats3: Int,
                          wColMaxSeats3: Int,

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
    distributeWorkshopFilling = true,

    // workshop planning CSV file
    wFile = new File(""),
    wDelimiter = ';',
    wRowsToSkip = 4,
    wNoTopics = 65,
    wColTopicId = 11, // 1-based A = 1
    wColTopicName = 13,
    wColCategory = 12,
    wColPreassignedTopic = 14,
    wColOnlyVoluntaryTopic = 15,
    wColGrades1 = 16,
    wColMinSeats1 = 19,
    wColMaxSeats1 = 20,
    wColGrades2 = 17,
    wColMinSeats2 = 19,
    wColMaxSeats2 = 20,
    wColGrades3 = 18,
    wColMinSeats3 = 19,
    wColMaxSeats3 = 20,

    // student topic selection CSV file
    sFile = new File(""),
    sDelimiter = ';',
    sRowsToSkip = 1,
    sNoStudents = 598,
    sColStudentId = 1,
    sColStudentName = 2,
    sColGrade = 3,
    sColFirstSelection = 12,

    // workshop assignments CSV file
    oDelimiter = ';',
  )

}