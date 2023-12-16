package hcd

import io.cvbio.collection.mutable.bimap.BiMap

/**
 * Workshop: A concrete workshop with a single timeslot, number of seats, room etc.
 * Workshop Choice: A workshop a student can chose. Typically behind each workshop choice there are 3 real workshops
 * with different timeslots.
 */
package object models {
  // Identifiers, with their own type for type safety, extending AnyVal for performance, i.e. no boxing/unboxing needed.

  /** ID of a workshop choice a student has, e.g. 1 to 50. */
  final case class WorkshopChoiceId(id: Int) extends AnyVal

  /** ID of a student, e.g. 1 to 1000. */
  final case class StudentId(id: Int) extends AnyVal

  /** Selection priority, e.g. 1 to 6. */
  final case class SelectionPriority(priority: Int) extends AnyVal

  /** ID of the workshop, e.g. 1 to 150. */
  final case class WorkshopId(id: Int) extends AnyVal

  /** Metric of a combo or distribution. */
  final case class Metric(metric: Int) extends AnyVal

  // Type safe enumerations
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

  /** Only the attributes of a possible workshop which are relevant to the algorithm to find the perfect distribution. */
  final case class PossibleWorkshop(category: Category, selectionPriority: SelectionPriority)

  // Only internally needed.
  private type PossibleWorkshopCandidate = (Workshop, SelectionPriority)

  // Mappings
  /**
   * Which workshop choice is selected with which priority.
   * BiMap guarantees both selection priority and workshop choice are unique.
   */
  type SelectedWorkshopChoices = BiMap[SelectionPriority, WorkshopChoiceId]

  /** The students' workshop choice selections. */
  type StudentsSelectedWorkshopChoices = Map[StudentId, SelectedWorkshopChoices]

  /** All the workshops. */
  type Workshops = Map[WorkshopId, Workshop]

  /** Workshops which generally match to selected workshop choices, with the selection priority. */
  type MatchingWorkshops = Map[WorkshopId, SelectionPriority]

  /** The matching workshops with selection priority for students. */
  type StudentsMatchingWorkshops = Map[StudentId, MatchingWorkshops]

  // Only internally needed.
  // A subset of workshops with represents a candidate for a possible combo of workshops
  // It contains all attributes and is too heavy for the distribution algorithm.
  type WorkshopComboCandidate = Map[WorkshopId, PossibleWorkshopCandidate]

  /** A subset of workshops with represents a possible combo of workshops and only contains the attributes relevant
   * to the algorithm to find the perfect distribution.
   */
  type WorkshopCombo = Map[WorkshopId, PossibleWorkshop]

  /** The assignments of students to a workshop. */
  type WorkshopAssignments = Map[WorkshopId, Set[StudentId]]
}
