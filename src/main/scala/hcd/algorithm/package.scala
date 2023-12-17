package hcd

import hcd.models._

package object algorithm {
  // Aggregate types

  protected[algorithm] final case class PossibleWorkshopCandidate(workshop: Workshop, selectionPriority: SelectionPriority)

  // Only the attributes of a possible workshop which are relevant to the algorithm to find the perfect distribution.
  protected[algorithm] final case class PossibleWorkshop(category: Category, selectionPriority: SelectionPriority)

  // Mappings

  // Workshops which generally match to selected workshop choices, with the selection priority.
  protected[algorithm] type MatchingWorkshops = Map[WorkshopId, SelectionPriority]

  // The matching workshops with selection priority for students.
  protected[algorithm] type StudentsMatchingWorkshops = Map[StudentId, MatchingWorkshops]

  // A subset of workshops with represents a candidate for a possible combo of workshops
  // It contains all attributes and is too heavy for the distribution algorithm.
  protected[algorithm] type WorkshopComboCandidate = Map[WorkshopId, PossibleWorkshopCandidate]

  // A subset of workshops with represents a possible combo of workshops and only contains the attributes relevant
  // to the algorithm to find the perfect distribution.
  protected[algorithm] type WorkshopCombo = Map[WorkshopId, PossibleWorkshop]
}
