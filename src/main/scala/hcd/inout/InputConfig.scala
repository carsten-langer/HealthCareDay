package hcd.inout

import java.io.File

case class InputConfig(
                        wFile: File,
                        wDelimiter: Char,
                        wRowsToSkip: Int,
                        wNoTopics: Int,
                        wColTopicId: Int, // 1-based A = 1
                        wColCategory: Int,
                        wColName: Int,
                        wColGrades1: Int,
                        wColSeats1: Int,
                        wColGrades2: Int,
                        wColSeats2: Int,
                        wColGrades3: Int,
                        wColSeats3: Int,
                      )
