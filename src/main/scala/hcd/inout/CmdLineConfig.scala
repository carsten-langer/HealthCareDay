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
                          wColSexes1: Int,
                          wColGrades1: Int,
                          wColMinSeats1: Int,
                          wColMaxSeats1: Int,
                          wColSexes2: Int,
                          wColGrades2: Int,
                          wColMinSeats2: Int,
                          wColMaxSeats2: Int,
                          wColSexes3: Int,
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
                          sColSex: Int,
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
    wColSexes1 = 16,
    wColGrades1 = 17,
    wColMinSeats1 = 22,
    wColMaxSeats1 = 23,
    wColSexes2 = 18,
    wColGrades2 = 19,
    wColMinSeats2 = 22,
    wColMaxSeats2 = 23,
    wColSexes3 = 20,
    wColGrades3 = 21,
    wColMinSeats3 = 22,
    wColMaxSeats3 = 23,

    // student topic selection CSV file
    sFile = new File(""),
    sDelimiter = ';',
    sRowsToSkip = 1,
    sNoStudents = 598,
    sColStudentId = 1,
    sColStudentName = 2,
    sColSex = 3,
    sColGrade = 4,
    sColFirstSelection = 13,

    // workshop assignments CSV file
    oDelimiter = ';',
  )

}