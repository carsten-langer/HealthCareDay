package hcd

import hcd.models._
import io.cvbio.collection.mutable.bimap.BiMap

package object algorithm {
  // Aggregate types

  // Collects temporarily all attributes of a workshop candidate which, if possible, could then be part of a  workshop combo.
  protected[algorithm] final case class WorkshopCandidate(topicId: TopicId, timeSlot: TimeSlot, category: Category, selectionPriority: SelectionPriority)

  // Only the attributes of a possible workshop which are relevant to the algorithm to find the perfect distribution.
  protected[algorithm] final case class PossibleWorkshop(category: Category, selectionPriority: SelectionPriority)

  // Mappings

  // Concrete workshops which generally match to selected workshop topics, with the selection priority.
  protected[algorithm] type MatchingWorkshops = Map[WorkshopId, SelectionPriority]

  // The matching workshops with selection priority for students.
  protected[algorithm] type StudentsMatchingWorkshops = Map[StudentId, MatchingWorkshops]

  // A subset of workshops with represents a candidate for a possible combo of workshops
  // It contains all attributes and is too heavy for the distribution algorithm.
  // BiMap guarantees both workshop id and workshop candidate are unique (for a single student).
  // This works as a workshop candidate is also unique by its combination of topicId and timeSlot.
  protected[algorithm] type WorkshopComboCandidate = BiMap[WorkshopId, WorkshopCandidate]

  // A subset of workshops with represents a possible combo of workshops and only contains the attributes relevant
  // to the algorithm to find the perfect distribution.
  // BiMap guarantees both workshop id and possible workshop are unique (for a single student).
  // This is true as long as we stick to only take concrete workshops as possible workshops which the student chose via
  // workshop topic id. Once we have to extend the distribution to include workshops on a topic that the student had
  // not selected, we have to think which selection priority we assign to such replacement workshops and if we then
  // have to remove the BiMap.
  protected[algorithm] type WorkshopCombo = BiMap[WorkshopId, PossibleWorkshop]

  class CounterPrinter {
    private var currentN = 0L
    private val startTime = System.currentTimeMillis()

    // currently takes ca. 44 s to calculate 1 combination for Student 410
    // i.e. try all combos for students 411, 412, ..., 999
    // with 90 workshop combos per student
    def countAndPrint(studentId: StudentId, workshopCombo: Seq[(WorkshopId, PossibleWorkshop)]): Unit = {
      currentN += 1L
      if (studentId.id < 411) {
        val now = System.currentTimeMillis()
        println(s"seconds spent: ${(now - startTime) / 1000}, currentN: $currentN, studentId: $studentId, workshopCombo: $workshopCombo")
      }
    }

  }

}
