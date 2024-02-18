package hcd.algorithms

import hcd.model._
import io.cvbio.collection.mutable.bimap.BiMap

package object fullcombinatoric {

  /** The seats that each workshop has. */
  private[fullcombinatoric] type WorkshopSeats = Map[WorkshopId, Seats]

  // Concrete workshops which generally match to selected workshop topics, with the selection priority.
  private[fullcombinatoric] type MatchingWorkshops = Map[WorkshopId, SelectionPriority]

  // The matching workshops with selection priority for students.
  private[fullcombinatoric] type StudentsMatchingWorkshops = Map[StudentId, MatchingWorkshops]

  // A subset of workshops with represents a candidate for a possible combo of workshops
  // It contains all attributes and is too heavy for the distribution algorithm.
  // BiMap guarantees both workshop id and workshop candidate are unique (for a single student).
  // This works as a workshop candidate is also unique by its combination of topicId and timeSlot.
  private[fullcombinatoric] type WorkshopComboCandidate = BiMap[WorkshopId, WorkshopCandidate]

  // A subset of workshops with represents a possible combo of workshops and only contains the attributes relevant
  // to the algorithm to find the perfect distribution.
  // BiMap guarantees both workshop id and possible workshop are unique (for a single student).
  // This is true as long as we stick to only take concrete workshops as possible workshops which the student chose via
  // workshop topic id. Once we have to extend the distribution to include workshops on a topic that the student had
  // not selected, we have to think which selection priority we assign to such replacement workshops and if we then
  // have to remove the BiMap.
  private[fullcombinatoric] type WorkshopCombo = BiMap[WorkshopId, PossibleWorkshop]

}
