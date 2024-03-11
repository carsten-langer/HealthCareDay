package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model.SelectionPriority.worstPrio
import hcd.model._

import scala.annotation.tailrec
import scala.util.Random

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: StoppableDistributionAlgorithm =
    initThenDistribute((shallStop: ShallStop) => distributeUntilStop(None, 1L, shallStop))

  /**
   * This algorithm's distribution function for a single round for testing.
   * From originally pre-ordered workshops and students, run a single round of distribution without shuffling,
   * ignoring the shallStop signal.
   */
  protected[randomroundrobin] def distributeSingleRound: DistributionAlgorithm =
    initThenDistribute(_ => distributeFromPreOrdered)(() => false)

  // Create an ordered base of workshops and students and run the given distribution function on them.
  private def initThenDistribute(distribute: ShallStop => DistributeFromPreOrdered): StoppableDistributionAlgorithm =
    (shallStop: ShallStop) =>
      (topics: Topics, workshops: Workshops) =>
        (studentsSelectedTopics: StudentsSelectedTopics) => {
          // Ordering of workshops and students is necessary for the unit tests to know the expected result.
          // Re-ordering, i.e. shuffling, both workshops and students is part of each round of the algorithm.

          // Have a list of workshops with a baseline ordering which is immutable between multiple rounds of the
          // algorithm, so that re-ordering it per round with different random seed is guaranteed to give reproducible
          // results.
          val baseOrderedWorkshops = workshops.toList.map {
            case (workshopId, (topicId, timeSlot, grades, seats)) =>
              Workshop(workshopId, topicId, timeSlot, grades, seats)
          }.sortBy(_.workshopId.id)

          // Have a list of students and their topic selections with a baseline ordering which is immutable between
          // multiple rounds of the algorithm, so that re-ordering it per round with different random seed is guaranteed
          // to give reproducible results. Each student has the topic selections represented as a list ordered by the
          // selection priority. The initial ordering between students is on the student id.
          val baseOrderedStudentsWithOrderedSelections = studentsSelectedTopics.toList.map {
            case (studentId, (grade, selectedTopics)) =>
              val topicSelections = selectedTopics.toList.map(_.swap).map(TopicSelection.tupled)
              val orderedTopicSelection = topicSelections.sortBy(_.selectionPriority.prio)
              Student(algoPrio = 1, studentId.id, studentId, grade, orderedTopicSelection, allTimeSlots, assignedTopics = Set.empty)
          }.sortBy(_.sortingOrder)

          distribute(shallStop)(topics, baseOrderedWorkshops, baseOrderedStudentsWithOrderedSelections)
        }

  // From originally pre-ordered workshops and students, run a distribution incl. shuffling until the  shallStop sign.
  @tailrec
  private def distributeUntilStop(maybeAccWorkshopAssignments: Option[WorkshopAssignments],
                                  round: Long,
                                  shallStop: ShallStop,
                                 )(
                                   topics: Topics,
                                   baseOrderedWorkshops: List[Workshop],
                                   baseOrderedStudents: List[Student],
                                 ): Option[WorkshopAssignments] =
    if (shallStop())
      maybeAccWorkshopAssignments
    else {
      logger.info(s"round: $round")
      distributeUntilStop(
        maybeAccWorkshopAssignments = shuffleThenDistribute(round)(topics, baseOrderedWorkshops, baseOrderedStudents),
        round + 1L,
        shallStop,
      )(
        topics,
        baseOrderedWorkshops,
        baseOrderedStudents,
      )
    }

  // From pre-ordered workshops and students create a new shuffled version and run the distribution.
  private def shuffleThenDistribute(seed: Long): DistributeFromPreOrdered =
    (topics: Topics, baseOrderedWorkshops: List[Workshop], baseOrderedStudents: List[Student]) => {
      Random.setSeed(seed)
      val shuffledWorkshops = Random.shuffle(baseOrderedWorkshops)
      val shuffledStudents = Random.shuffle(baseOrderedStudents)
        .zipWithIndex
        .map { case (student, sortingOrder) => student.copy(sortingOrder = sortingOrder) }
      distributeFromPreOrdered(topics, shuffledWorkshops, shuffledStudents)
    }

  // This algorithm's distribution function for one round from pre-ordered workshops and students.
  private val distributeFromPreOrdered: DistributeFromPreOrdered =
    (topics: Topics, orderedWorkshops: List[Workshop], orderedStudents: List[Student]) => {

      def hasNot3TimesGivenCategory(topicCandidates: Set[TopicId], category: Category) =
        topicCandidates.toList.map(topics).count(_ == category) < 3

      def haveMinVaryingCategories(topicCandidates: Set[TopicId]): Boolean =
        hasNot3TimesGivenCategory(topicCandidates, Nutrition) && hasNot3TimesGivenCategory(topicCandidates, Relaxation)

      def haveMaxVaryingCategories(topicCandidates: Set[TopicId]): Boolean =
        haveMinVaryingCategories(topicCandidates) && hasNot3TimesGivenCategory(topicCandidates, Sports)

      // First round of distribution: For a student, select the next workshop being part of her selection and which
      // otherwise fulfils all criteria.
      def findWorkshopId1: FindWorkshopId12 = (student: Student, workshopAssignments: WorkshopAssignments) => {
        object ExtractorWorkshopForTopic {
          def unapply(topicSelection: TopicSelection): Option[Holder[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]] =
            orderedWorkshops.collectFirst {
              case Workshop(workshopId, topicSelection.topicId, timeSlot, grades, seats)
                if student.unassignedTimeSlots.contains(timeSlot) &&
                  grades.contains(student.grade) &&
                  workshopAssignments.getOrElse(workshopId, Set.empty).size < seats.n &&
                  haveMaxVaryingCategories(student.assignedTopics + topicSelection.topicId) =>
                logger.trace(s"found1: $workshopId at $timeSlot for $student.")
                Holder((workshopId, topicSelection.topicId, topicSelection.selectionPriority, timeSlot))
            }
        }

        student.topicSelections.collectFirst {
          case ExtractorWorkshopForTopic(Holder((workshopId, topicId, selectionPriority, timeSlot))) =>
            (workshopId, topicId, selectionPriority, timeSlot)
        }
      }

      // First and second round of distribution:
      // For each student, select the next workshop which fulfills the criteria given in findWorkshopId function.
      // If no workshop can be found, skip the student and leave the distribution to the next round.
      @tailrec
      def recursion12(findWorkshopId: FindWorkshopId12)(
        workshopAssignments: WorkshopAssignments,
        undistributableStudents: List[Student],
        studentsToDistribute: List[Student],
      ): Option[(WorkshopAssignments, List[Student])] =
        studentsToDistribute match {
          case Nil =>
            logger.debug("Successful end of recursion12.")
            Some((workshopAssignments, undistributableStudents))
          case ::(headStudent@Student(algoPrio, _, studentId, _, topicSelections, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId(headStudent, workshopAssignments) match {
              case None =>
                // skip this student as no workshops could be found now, the student will get assigned workshops from next round.
                val updatedUndistributableStudents = undistributableStudents :+ headStudent
                recursion12(findWorkshopId)(workshopAssignments, updatedUndistributableStudents, nextStudents)
              case Some((foundWorkshopId, foundTopicId, SelectionPriority(prio), foundTimeSlot)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val (_, updatedTopicSelections) = topicSelections.span(_.selectionPriority.prio <= prio)
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      algoPrio = algoPrio + worstPrio.prio - prio,
                      topicSelections = updatedTopicSelections,
                      unassignedTimeSlots = updatedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    // While it does not make a difference for the resulting total order if we write
                    // (updatedStudent :: nextStudents).sortBy() or
                    // (nextStudents :+ updatedStudent).sortBy(), as long as the student.sortingOrder is unique,
                    // the latter expresses better that students just assigned go the back of the list.
                    // It may also have a performance improvement if the uses sorting algorithm handles nearly-sorted
                    // lists well.
                    (nextStudents :+ updatedStudent).sortBy(s => (s.algoPrio, s.sortingOrder))
                  }
                recursion12(findWorkshopId)(updatedWorkshopAssignments, undistributableStudents, updatedStudents)
            }
        }

      val maybeDistribution1 = recursion12(findWorkshopId1)(
        workshopAssignments = Map.empty,
        undistributableStudents = List.empty,
        studentsToDistribute = orderedStudents
      )
      logger.debug(s"maybeDistribution1: $maybeDistribution1")

      // Second or third round of distribution: For each student, select the next workshop which fulfils all mandatory
      // criteria and all criteria by the given function hasVaryingCategories, regardless of the student's selection.
      def findWorkshopId23(haveVaryingCategories: Set[TopicId] => Boolean): FindWorkshopId23 = (student: Student, workshopAssignments: WorkshopAssignments) =>
        orderedWorkshops.collectFirst {
          case Workshop(workshopId, topicId, timeSlot, grades, seats)
            if student.unassignedTimeSlots.contains(timeSlot) &&
              !student.assignedTopics.contains(topicId) &&
              grades.contains(student.grade) &&
              workshopAssignments.getOrElse(workshopId, Set.empty).size < seats.n &&
              haveVaryingCategories(student.assignedTopics + topicId) =>
            logger.trace(s"found23: $workshopId at $timeSlot for $student.")
            (workshopId, topicId, timeSlot)
        }

      // Second round of distribution: adopt findWorkshopId23 to the signature needed in recursion12.
      def findWorkshopId2: FindWorkshopId12 = (student: Student, workshopAssignments: WorkshopAssignments) =>
        findWorkshopId23(haveMaxVaryingCategories)(student, workshopAssignments).map {
          case (workshopId, topicId, timeSlot) =>
            (workshopId, topicId, SelectionPriority(Int.MaxValue), timeSlot)
        }

      // Second round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection.
      val maybeDistribution2 = maybeDistribution1.flatMap { case (workshopAssignments, notYetDistributedStudents) =>
        recursion12(findWorkshopId2)(
          workshopAssignments,
          undistributableStudents = List.empty,
          studentsToDistribute = notYetDistributedStudents
        )
      }
      logger.debug(s"maybeDistribution2: $maybeDistribution2")

      // Third round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection, with the exception of the criteria that no 3 workshops of category sports shall be assigned.
      def findWorkshopId3: FindWorkshopId23 = findWorkshopId23(haveMinVaryingCategories)

      // Third round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection, with the exception of the criteria that no 3 workshops of category sports shall be assigned.
      // If no workshop can be found, the distribution fails.
      @tailrec
      def recursion3(workshopAssignments: WorkshopAssignments, remainingStudents: List[Student]): Option[WorkshopAssignments] =
        remainingStudents match {
          case Nil =>
            logger.debug("Successful end of recursion2.")
            Some(workshopAssignments)
          case ::(headStudent@Student(_, _, studentId, _, _, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId3(headStudent, workshopAssignments) match {
              case None =>
                logger.debug(s"Unsuccessful end of recursion3. No suitable workshop found for student $headStudent.")
                None
              case Some((foundWorkshopId, foundTopicId, foundTimeSlot)) =>
                val updatedWorkshopAssignments = workshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      unassignedTimeSlots = updatedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    updatedStudent :: nextStudents
                  }
                recursion3(updatedWorkshopAssignments, updatedStudents)
            }
        }

      val maybeWorkshopAssignments3 = maybeDistribution2.flatMap((recursion3 _).tupled)
      logger.debug(s"maybeWorkshopAssignments3: $maybeWorkshopAssignments3")

      // make sure each workshop has a set of students. If not yet the case, add an empty set.
      maybeWorkshopAssignments3.map(workshopAssignments =>
        orderedWorkshops.foldLeft(workshopAssignments) { case (accWorkshopAssignments, workshop) =>
          accWorkshopAssignments.updatedWith(workshop.workshopId) {
            case None => Some(Set.empty[StudentId])
            case someValue => someValue
          }
        }
      )

    }

  // Ordering the Workshops is necessary for the unit tests to know the expected result.
  // It is easier if we have our own data type.
  private final case class Workshop(workshopId: WorkshopId, topicId: TopicId, timeSlot: TimeSlot, grades: Set[Grade], seats: Seats)

  // Ordering the SelectedTopics per student is necessary for the unit tests to know the expected result.
  // It is easier if we have our own data type. Ordering makes most sense by selection priority,
  // thus we use the flipped order of values compared to SelectedTopics.
  private final case class TopicSelection(selectionPriority: SelectionPriority, topicId: TopicId)

  private final case class Student(
                                    algoPrio: Int,
                                    sortingOrder: Int,
                                    studentId: StudentId,
                                    grade: Grade,
                                    topicSelections: List[TopicSelection],
                                    unassignedTimeSlots: Set[TimeSlot],
                                    assignedTopics: Set[TopicId],
                                  )

  private type DistributeFromPreOrdered = (Topics, List[Workshop], List[Student]) => Option[WorkshopAssignments]
  private type FindWorkshopId12 = (Student, WorkshopAssignments) => Option[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]
  private type FindWorkshopId23 = (Student, WorkshopAssignments) => Option[(WorkshopId, TopicId, TimeSlot)]

  // See https://github.com/scala/bug/issues/6675 and https://github.com/scala/bug/issues/6111
  // for the need for a holder to avoid deprecation message on (scala/bug#6675)
  private case class Holder[T](_1: T) extends Product1[T]

}
