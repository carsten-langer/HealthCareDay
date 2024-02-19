package hcd.inout

import java.io.File

case class InputConfig(
                        // workshop planning CSV file
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

                        // student topic selection CSV file
                        sFile: File,
                        sDelimiter: Char,
                        sRowsToSkip: Int,
                        sNoStudents: Int,
                        sColStudentId: Int,
                        sColGrade: Int,
                        sColFirstSelection: Int,
                      )
