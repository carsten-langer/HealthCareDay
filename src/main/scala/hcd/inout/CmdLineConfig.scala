package hcd.inout

import hcd.algorithms.Algorithm
import hcd.model.Flag

import java.io.File
import scala.concurrent.duration.Duration

case class CmdLineConfig(
                          algorithm: Algorithm,
                          searchDuration: Duration,
                          initialSeed: Long,
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
