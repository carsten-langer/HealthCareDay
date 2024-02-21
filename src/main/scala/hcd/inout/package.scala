package hcd

import java.io.File

package object inout {

  val DefaultInputConfig: InputConfig = InputConfig(
    // workshop planning CSV file
    wFile = new File(""),
    wDelimiter = ';',
    wRowsToSkip = 5,
    wNoTopics = 53,
    wColTopicId = 1, // 1-based A = 1
    wColCategory = 2,
    wColName = 3,
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
    sColGrade = 28,
    sColFirstSelection = 22,
  )

}
