package hcd

import io.cvbio.collection.mutable.bimap.BiMap

/**
 * Workshop: A concrete workshop with a single timeslot, number of seats, room etc., dealing with one topic.
 * Topic: A workshop topic a student can chose. Typically behind each workshop topic there are 3 concrete workshops
 * with different timeslots.
 */
package object model {

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

  /** The assignments of workshops to a student. */
  private type StudentAssignments = Map[StudentId, Set[WorkshopId]]

  /**
   * An algorithm to distribute students to workshops based on their topic selections.
   *
   * Type parameter A: An extra return value, if available. (@tparam does not work for scaladoc)
   */
  type DistributionAlgorithm[A] = (Topics, Workshops, WorkshopSeats) => StudentsSelectedTopics => Option[(WorkshopAssignments, A)]

  def studentAssignmentsFrom(workshopAssignments: WorkshopAssignments): StudentAssignments =
    workshopAssignments
      .toList
      .flatMap { case (workshopId, students) => students.map(studentId => (studentId, workshopId)) }
      .groupMap { case (studentId, _) => studentId } { case (_, workshopId) => workshopId }
      .view
      .mapValues(_.toSet)
      .toMap

}
