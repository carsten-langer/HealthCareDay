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

  /** All topics with their name and category. */
  type TopicsWithName = Map[TopicId, (String, Category)]

  /**
   * Which workshop topic is selected with which priority (per student).
   * BiMap guarantees both topic and selection priority are unique (per student).
   */
  type SelectedTopics = BiMap[TopicId, SelectionPriority]

  /** The students' grade and workshop topic selections. */
  type StudentsSelectedTopics = Map[StudentId, (Grade, SelectedTopics)]

  /** The students' name and grade and workshop topic selections. */
  type StudentsSelectedTopicsWithName = Map[StudentId, (String, Grade, SelectedTopics)]

  /**
   * All the concrete workshops.
   * The relation between WorkshopId and Topic/TimeSlot is actually a BiMap, i.e. both workshop id and combination of
   * topic id and timeslot is unique within this map.
   * That is: each topic can only exist once per timeslot, and such topic/timeslot combination is a unique concrete
   * workshop.
   * However, as we extend the workshops with other attributes relevant to the distribution algorithms, this would break
   * the the nature of a BiMap, so it is not used. Instead, this property of the map can be checked via verification.
   */
  type Workshops = Map[WorkshopId, (TopicId, TimeSlot, Set[Grade], Seats)]

  /** The assignments of students to a workshop. */
  type WorkshopAssignments = Map[WorkshopId, Set[StudentId]]

  /** The assignments of workshops to a student. */
  private type StudentAssignments = Map[StudentId, Set[WorkshopId]]

  /** An algorithm to distribute students to workshops based on their topic selections. */
  type DistributionAlgorithm = (Topics, Workshops) => StudentsSelectedTopics => Option[WorkshopAssignments]

  /** Indicates to stop if result is `true`. */
  type ShallStop = () => Boolean

  /**
   * An algorithm to distribute students to workshops based on their topic selections,
   * which stops when a given function indicates so.
   */
  type StoppableDistributionAlgorithm = ShallStop => DistributionAlgorithm

  /**
   * An algorithm to distribute students to workshops based on their topic selections,
   * which stops when a given function indicates so and has the option to save intermediate states.
   */
  type StoppableDistributionAlgorithmSavingIntermediateStates = (WorkshopAssignments => Unit) => StoppableDistributionAlgorithm

  def topicsFrom(topicsWithName: TopicsWithName): Topics =
    topicsWithName.view.mapValues { case (_, category) => category }.toMap

  def studentsSelectedTopicsFrom(studentsSelectedTopicsWithName: StudentsSelectedTopicsWithName): StudentsSelectedTopics =
    studentsSelectedTopicsWithName.view.mapValues { case (_, grade, selectedTopics) => (grade, selectedTopics) }.toMap

  def studentAssignmentsFrom(workshopAssignments: WorkshopAssignments): StudentAssignments =
    workshopAssignments
      .toList
      .flatMap { case (workshopId, students) => students.map(studentId => (studentId, workshopId)) }
      .groupMap { case (studentId, _) => studentId } { case (_, workshopId) => workshopId }
      .view
      .mapValues(_.toSet)
      .toMap

  val allTimeSlots: Set[TimeSlot] = TimeSlot.values.toSet

}
