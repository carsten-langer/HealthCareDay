package hcd

import io.cvbio.collection.mutable.bimap.BiMap

/**
 * Workshop: A concrete workshop with a single timeslot, number of seats, room etc., dealing with one topic.
 * Topic: A workshop topic a student can chose. Typically behind each workshop topic there are 3 concrete workshops
 * with different timeslots.
 */
package object models {
  // Identifiers, with their own type for type safety, extending AnyVal for performance, i.e. no boxing/unboxing needed.

  /** ID of a workshop topic a student can chose from, e.g. 1 to 50. */
  final case class TopicId(id: Int) extends AnyVal

  /** ID of a student, e.g. 1 to 1000. */
  final case class StudentId(id: Int) extends AnyVal

  /** Selection priority, e.g. 1 to 6. */
  final case class SelectionPriority(prio: Int) extends AnyVal

  /** ID of a concrete workshop, e.g. 1 to 150. */
  final case class WorkshopId(id: Int) extends AnyVal

  /** Number of workshop seats, e.g. 20. */
  final case class Seats(n: Int) extends AnyVal

  /** Metric of a combo or distribution. */
  final case class Metric(metric: Int) extends AnyVal

  // Type safe enumerations
  sealed trait Category

  final case object Nutrition extends Category

  final case object Relaxation extends Category

  final case object Sports extends Category

  sealed trait TimeSlot

  final case object FirstTimeSlot extends TimeSlot

  final case object SecondTimeSlot extends TimeSlot

  final case object ThirdTimeSlot extends TimeSlot

  // Aggregate types

  /** A workshop is determined by a topic and a timeslot. This combination determines a concrete workshop. */
  final case class TopicTimeslot(topicId: TopicId, timeSlot: TimeSlot)

  // Mappings

  /** All topics with their category. */
  type Topics = Map[TopicId, Category]

  /**
   * Which workshop topic is selected with which priority (per student).
   * BiMap guarantees both topic and selection priority are unique (per student).
   */
  type SelectedTopics = BiMap[TopicId, SelectionPriority]

  /** The students' workshop topic selections. */
  type StudentsSelectedTopics = Map[StudentId, SelectedTopics]

  /**
   * All the concrete workshops.
   * BiMap guarantees both workshop id and combination of topic id and timeslot is unique.
   * That is: each topic can only exist once per timeslot, and such topic/timeslot combination is a unique concrete
   * workshop.
   */
  type Workshops = BiMap[WorkshopId, TopicTimeslot]

  /** The seats that each workshop has. */
  type WorkshopSeats = Map[WorkshopId, Seats]

  /** The assignments of students to a workshop. */
  type WorkshopAssignments = Map[WorkshopId, Set[StudentId]]
}
