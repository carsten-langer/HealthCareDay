package hcd.algorithms.fullcombinatoric

import hcd.model.{StudentId, WorkshopId}

// A debugging printer
private final class CounterLogger(log: String => Unit) {
  private val everyN = 10L * 1000L * 1000L
  private var currentN = 0L
  private var currentEveryN = 0L
  private val startTime = System.currentTimeMillis()

  // The calculation speed for 1 student is not stable anymore as the free workshop seats are taken into account
  // which makes it a bit random how deep the recursion has to go down the combination tree before filling up all
  // workshop seats.
  // However, some measurements indicate 2,105,123,739 in 205 s, i.e. 10,268,896 calls per second.
  def countAndLog(studentId: StudentId, workshopCombo: => Seq[WorkshopId]): Unit = {
    currentN += 1L
    if (currentN > currentEveryN) {
      val now = System.currentTimeMillis()
      log(s"seconds spent: ${(now - startTime) / 1000}, currentN: $currentN, studentId: $studentId, workshopCombo: $workshopCombo")
      currentEveryN = currentEveryN + everyN
    }
  }
}
