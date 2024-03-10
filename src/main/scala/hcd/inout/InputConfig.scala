package hcd.inout

import hcd.algorithms.Algorithm

import java.io.File
import scala.concurrent.duration.Duration

case class InputConfig(
                        algorithm: Algorithm,
                        searchDuration: Duration,

                        // workshop planning CSV file
                        wFile: File,
                        wDelimiter: Char,
                        wRowsToSkip: Int,
                        wNoTopics: Int,
                        wColTopicId: Int, // 1-based A = 1
                        wColTopicName: Int,
                        wColCategory: Int,
                        wColGrades1: Int,
                        wColSeats1: Int,
                        wColGrades2: Int,
                        wColSeats2: Int,
                        wColGrades3: Int,
                        wColSeats3: Int,
                        wFullDayTopics: Seq[Int],

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
