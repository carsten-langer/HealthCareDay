package hcd

import java.io.File

package object inout {

  val DefaultInputConfig: InputConfig = InputConfig(
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
  )

}
