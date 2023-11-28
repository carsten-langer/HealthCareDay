package hcd

import io.cvbio.collection.mutable.bimap.BiMap

package object models {
  // Identifiers, with their own type for type safety, extending AnyVal for performance, i.e. no boxing/unboxing needed
  final case class WorkshopChoiceId(id: Int) extends AnyVal

  final case class StudentId(id: Int) extends AnyVal

  final case class SelectionPriority(priority: Int) extends AnyVal

  final case class WorkshopId(id: Int) extends AnyVal

  // type safe enumerations
  sealed trait Category

  final case object Health extends Category

  final case object Relaxation extends Category

  final case object Sports extends Category

  sealed trait TimeSlot

  final case object FirstTimeSlot extends TimeSlot

  final case object SecondTimeSlot extends TimeSlot

  final case object ThirdTimeSlot extends TimeSlot

  // Aggregate types

  /** All attributes to a workshop. */
  final case class Workshop(category: Category, choiceId: WorkshopChoiceId, timeSlot: TimeSlot, seats: Int)

  /** All attributes of a selected workshop. */
  final case class SelectedWorkshop(category: Category, choiceId: WorkshopChoiceId, timeSlot: TimeSlot, selectionPriority: SelectionPriority)

  /** Only the attributes to possible workshop which are relevant to the algorithm to find the perfect distribution. */
  final case class PossibleWorkshop(category: Category, selectionPriority: SelectionPriority)

  // Mappings
  /**
   * Which workshop choice is selected with which priority.
   * BiMap guarantees both selection priority and workshop choice are unique.
   */
  type WorkshopSelection = BiMap[SelectionPriority, WorkshopChoiceId]

  /** The prioritized workshops selections for students. */
  type StudentWorkshopSelections = Map[StudentId, WorkshopSelection]

  /** All the workshops. */
  type Workshops = Map[WorkshopId, Workshop]

  /** A subset of workshops with the attributes of selected workshops. */
  type SelectedWorkshops = Map[WorkshopId, SelectedWorkshop]

  /** A subset of workshops with only the attributes relevant to the algorithm to find the perfect distribution. */
  type PossibleWorkshops = Map[WorkshopId, PossibleWorkshop]

  /** The assignments of students to a workshop. */
  type WorkshopAssignments = Map[WorkshopId, Set[StudentId]]
}
