package hcd.algorithms.randomroundrobin

import com.typesafe.scalalogging.StrictLogging
import hcd.model
import hcd.model.Metric.metricGlobal
import hcd.model.SelectionPriority.WorstPrio
import hcd.model._

import scala.annotation.tailrec
import scala.util.Random

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: InitiallySeededStoppableDistributionAlgorithmSavingIntermediateStates =
    (initialSeed: Long) =>
      (saveIntermediateState: WorkshopAssignments => Unit) =>
        (shallStop: ShallStop) =>
          (distributeWorkshopFilling: DistributeWorkshopFilling, topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics) =>
            initThenDistribute(
              distributeUntilStop(initialSeed, saveIntermediateState, shallStop, workshops, studentsSelectedTopics)
            )(distributeWorkshopFilling, topics, workshops, studentsSelectedTopics)

  /**
   * This algorithm's distribution function for a single round for testing.
   * From originally pre-ordered workshops and students, run a single round of distribution without shuffling,
   * ignoring the shallStop signal.
   */
  protected[randomroundrobin] def distributeSingleRound: DistributionAlgorithm =
    initThenDistribute(distributeFromPreOrdered)

  // Create an ordered base of workshops and students and run the given distribution function on them.
  private def initThenDistribute(distributeFromPreOrdered: DistributeFromPreOrdered): DistributionAlgorithm =
    (distributeWorkshopFilling: DistributeWorkshopFilling, topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics) => {
      // Ordering of workshops and students is necessary for the unit tests to know the expected result.
      // Re-ordering, i.e. shuffling, both workshops and students is part of each round of the algorithm.

      // Have a list of workshops with a baseline ordering which is immutable between multiple rounds of the
      // algorithm, so that re-ordering it per round with different random seed is guaranteed to give reproducible
      // results.
      val baseWorkshops = workshops.toList.map {
        case (workshopId, (topicId, timeSlot, grades, seats)) => Workshop(workshopId, topicId, timeSlot, grades, seats)
      }
      val baseOrderedWorkshops = baseWorkshops.sortBy(_.workshopId.id)

      // Have a list of students and their topic selections with a baseline ordering which is immutable between
      // multiple rounds of the algorithm, so that re-ordering it per round with different random seed is guaranteed
      // to give reproducible results. Each student has the topic selections represented as a list ordered by the
      // selection priority. The initial ordering between students is on the student id.
      val baseStudentsWithOrderedSelections = studentsSelectedTopics.toList.map {
        case (studentId, (grade, selectedTopics)) =>
          val topicSelections = selectedTopics.toList.map(_.swap).map(TopicSelection.tupled)
          val orderedTopicSelections = topicSelections.sortBy(_.selectionPriority.prio)
          Student(
            algoPrio = 1,
            sortingOrder = studentId.id,
            studentId = studentId,
            grade = grade,
            orderedTopicSelections = orderedTopicSelections,
            unassignedTimeSlots = model.allTimeSlots,
            assignedTopics = Set.empty,
          )
      }
      val baseOrderedStudentsWithOrderedSelections = baseStudentsWithOrderedSelections.sortBy(_.sortingOrder)

      distributeFromPreOrdered(distributeWorkshopFilling, topics, baseOrderedWorkshops, baseOrderedStudentsWithOrderedSelections)
    }

  // From originally pre-ordered workshops and students, run a distribution incl. shuffling until the shallStop sign.
  private def distributeUntilStop(
                                   initialSeed: Seed,
                                   saveIntermediateState: WorkshopAssignments => Unit,
                                   shallStop: ShallStop,
                                   workshops: Workshops,
                                   studentsSelectedTopics: StudentsSelectedTopics,
                                 ): DistributeFromPreOrdered =
    (distributeWorkshopFilling: DistributeWorkshopFilling, topics: Topics, baseOrderedWorkshops: List[Workshop], baseOrderedStudents: List[Student]) => {

      val WorstMetric = Int.MaxValue
      val startNanoTime = System.nanoTime()

      // From originally pre-ordered workshops and students, run a distribution incl. shuffling until the shallStop sign.
      @tailrec
      def _distributeUntilStop(maybeCurrentWorkshopAssignments: Option[WorkshopAssignments],
                               maybeBestWorkshopAssignments: Option[WorkshopAssignments],
                               bestMetric: Int,
                               round: Long,
                              ): Option[WorkshopAssignments] = {
        def secondsUntilNow: Long = (System.nanoTime() - startNanoTime) / 1_000_000_000L

        def calcCurrentGlobalMetric(workshopAssignments: WorkshopAssignments): Int =
          metricGlobal(topics, workshops, studentsSelectedTopics)(workshopAssignments).m

        if (shallStop()) {
          logger.info(s"requested to stop at round $round after $secondsUntilNow seconds.")
          maybeBestWorkshopAssignments
        } else {
          val currentGlobalMetric = maybeCurrentWorkshopAssignments
            .map(calcCurrentGlobalMetric)
            .getOrElse(WorstMetric)
          val (nextMetric, nextMaybeBestWorkshopAssignments) =
            if (currentGlobalMetric < bestMetric) {
              logger.info(s"found better metric $currentGlobalMetric at round $round after $secondsUntilNow seconds.")
              maybeCurrentWorkshopAssignments.foreach(saveIntermediateState)
              (currentGlobalMetric, maybeCurrentWorkshopAssignments)
            } else
              (bestMetric, maybeBestWorkshopAssignments)
          val nextMaybeCurrentWorkshopAssignments =
            shuffleThenDistribute(initialSeed + round)(distributeWorkshopFilling, topics, baseOrderedWorkshops, baseOrderedStudents)
          _distributeUntilStop(
            nextMaybeCurrentWorkshopAssignments,
            nextMaybeBestWorkshopAssignments,
            nextMetric,
            round + 1L,
          )
        }
      }

      _distributeUntilStop(
        maybeCurrentWorkshopAssignments = None,
        maybeBestWorkshopAssignments = None,
        bestMetric = WorstMetric,
        round = 0L,
      )

    }

  // From pre-ordered workshops and students create a new shuffled version and run the distribution.
  private def shuffleThenDistribute(seed: Seed): DistributeFromPreOrdered =
    (distributeWorkshopFilling: DistributeWorkshopFilling, topics: Topics, baseOrderedWorkshops: List[Workshop], baseOrderedStudents: List[Student]) => {
      Random.setSeed(seed)
      val shuffledWorkshops = Random.shuffle(baseOrderedWorkshops)
      val shuffledStudents = Random.shuffle(baseOrderedStudents)
        .zipWithIndex
        .map { case (student, index) => student.copy(sortingOrder = index) }
      distributeFromPreOrdered(distributeWorkshopFilling, topics, shuffledWorkshops, shuffledStudents)
    }

  // This algorithm's distribution function for one round from pre-ordered workshops and students.
  private val distributeFromPreOrdered: DistributeFromPreOrdered =
    (distributeWorkshopFilling: DistributeWorkshopFilling, topics: Topics, orderedWorkshops: List[Workshop], orderedStudents: List[Student]) => {

      val (preassignedWorkshops, normalAndOnlyVoluntaryWorkshops) = orderedWorkshops.partition { workshop =>
        val (_, _, preassigned, _) = topics(workshop.topicId)
        preassigned
      }
      val normalWorkshops = normalAndOnlyVoluntaryWorkshops.filterNot { workshop =>
        val (_, _, _, onlyVoluntary) = topics(workshop.topicId)
        onlyVoluntary
      }

      def hasNot3TimesGivenCategory(topicCandidates: Set[TopicId], category: Category): Boolean =
        topicCandidates.toList.map(topics).count { case (_, thisCategory, _, _) => thisCategory == category } < 3

      def haveMinVaryingCategories(topicCandidates: Set[TopicId]): Boolean =
        hasNot3TimesGivenCategory(topicCandidates, Nutrition) && hasNot3TimesGivenCategory(topicCandidates, Relaxation)

      def haveMaxVaryingCategories(topicCandidates: Set[TopicId]): Boolean =
        haveMinVaryingCategories(topicCandidates) && hasNot3TimesGivenCategory(topicCandidates, Sports)

      // Collect best workshop from given list of workshops which fulfills some mandatory criteria and also
      // the given criteria on the set of to-be-assigned topics.
      def collectBestWorkshop(
                               workshops: List[Workshop],
                               isAssignable: Set[TopicId] => Boolean,
                             )(
                               student: Student,
                               workshopAssignments: WorkshopAssignments,
                             )(
                               topicSelection: TopicSelection,
                             )
      : Option[Holder[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]] = {
        val possibleWorkshops = workshops.flatMap {
          case Workshop(workshopId, topicId, timeSlot, grades, seats) =>
            val filledSeats = workshopAssignments.getOrElse(workshopId, Set.empty).size
            if (
              topicId == topicSelection.topicId
                && student.unassignedTimeSlots.contains(timeSlot)
                && grades.contains(student.grade)
                && filledSeats < seats.n
                && isAssignable(student.assignedTopics + topicId)
            ) {
              logger.trace(s"found: $workshopId at $timeSlot for $student.")
              Some((Holder((workshopId, topicId, topicSelection.selectionPriority, timeSlot)), filledSeats.toDouble / seats.n))
            } else None
        }
        val maybeWorkshop = if (distributeWorkshopFilling)
          possibleWorkshops.minByOption { case (_, fillRatio) => fillRatio }
        else possibleWorkshops.headOption
        maybeWorkshop.map { case (workshopHolder, _) => workshopHolder }
      }

      // Initial distribution for pre-assigned topics: For each student, select the next workshop which corresponds to
      // the pre-assigned topic.
      def findWorkshopId0: FindWorkshopId = (student: Student, workshopAssignments: WorkshopAssignments) => {
        object ExtractorFindWorkshopForTopic {
          def unapply(topicSelection: TopicSelection): Option[Holder[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]] =
            collectBestWorkshop(preassignedWorkshops, isAssignable = _ => true)(student, workshopAssignments)(topicSelection)
        }

        student.orderedTopicSelections.collectFirst { case ExtractorFindWorkshopForTopic(Holder(workshopTuple)) => workshopTuple }
      }

      // The initial round of distribution only for pre-assigned topics:
      // For each student who selected a pre-assigned topic, select the first corresponding workshop.
      // Leave everything else to the next round.
      @tailrec
      def recursion0(
                      accWorkshopAssignments: WorkshopAssignments,
                      accUndistributableStudents: List[Student],
                      remainingStudentsToDistribute: List[Student],
                    ): Option[(WorkshopAssignments, List[Student])] =
        remainingStudentsToDistribute match {
          case Nil =>
            logger.debug("Successful end of recursion0.")
            Some((accWorkshopAssignments, accUndistributableStudents))
          case ::(headStudent@Student(_, _, studentId, _, orderedTopicSelections, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId0(headStudent, accWorkshopAssignments) match {
              case None =>
                // skip this student as no pre-assigned workshops could be found anymore (or at all), the student will get assigned workshops from next round.
                val updatedUndistributableStudents = accUndistributableStudents :+ headStudent
                recursion0(accWorkshopAssignments, updatedUndistributableStudents, nextStudents)
              case Some((foundWorkshopId, foundTopicId, _, foundTimeSlot)) =>
                val updatedWorkshopAssignments = accWorkshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedUnassignedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedUnassignedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val updatedTopicSelections = orderedTopicSelections.filterNot(_.topicId == foundTopicId)
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      orderedTopicSelections = updatedTopicSelections,
                      unassignedTimeSlots = updatedUnassignedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    updatedStudent :: nextStudents
                  }
                recursion0(updatedWorkshopAssignments, accUndistributableStudents, updatedStudents)
            }
        }

      val maybeDistribution0 = recursion0(
        accWorkshopAssignments = Map.empty,
        accUndistributableStudents = List.empty,
        remainingStudentsToDistribute = orderedStudents,
      )
      logger.debug(s"maybeDistribution0: $maybeDistribution0")
      maybeDistribution0.foreach { case (_, students) =>
        students.foreach(student =>
          student
            .orderedTopicSelections
            .map { topicSelection =>
              val topicId = topicSelection.topicId
              topics.get(topicId).foreach { case (_, _, preassigned, _) =>
                if (preassigned)
                  logger.error(s"Student ${student.studentId} could not be pre-assigned to topic $topicId; check prerequisites like allowed graded and number of free seats!")
              }
            }
        )
      }

      // First round of distribution: For a student, select the next workshop being part of her selection and which
      // otherwise fulfils all criteria.
      // During the first round a student can only get assigned a topic which she selected, thus find a workshop from
      // both normal and only-voluntary workshops.
      def findWorkshopId1: FindWorkshopId = (student: Student, workshopAssignments: WorkshopAssignments) => {
        object ExtractorFindWorkshopForTopic {
          def unapply(topicSelection: TopicSelection): Option[Holder[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]] =
            collectBestWorkshop(normalAndOnlyVoluntaryWorkshops, haveMaxVaryingCategories)(student, workshopAssignments)(topicSelection)
        }

        // Find the workshop with best priority for the given student that fulfills all other criteria.
        student.orderedTopicSelections.collectFirst { case ExtractorFindWorkshopForTopic(Holder(workshopTuple)) => workshopTuple }
      }

      // First and second round of distribution:
      // For each student, select the next workshop which fulfills the criteria given in findWorkshopId function.
      // If no workshop can be found, skip the student and leave the distribution to the next round.
      @tailrec
      def recursion12(findWorkshopId: FindWorkshopId)(
        accWorkshopAssignments: WorkshopAssignments,
        accUndistributableStudents: List[Student],
        remainingStudentsToDistribute: List[Student],
      ): Option[(WorkshopAssignments, List[Student])] =
        remainingStudentsToDistribute match {
          case Nil =>
            logger.debug("Successful end of recursion12.")
            Some((accWorkshopAssignments, accUndistributableStudents))
          case ::(headStudent@Student(algoPrio, _, studentId, _, orderedTopicSelections, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId(headStudent, accWorkshopAssignments) match {
              case None =>
                // skip this student as no workshops could be found now, the student will get assigned workshops from next round.
                val updatedUndistributableStudents = accUndistributableStudents :+ headStudent
                recursion12(findWorkshopId)(accWorkshopAssignments, updatedUndistributableStudents, nextStudents)
              case Some((foundWorkshopId, foundTopicId, SelectionPriority(prio), foundTimeSlot)) =>
                val updatedWorkshopAssignments = accWorkshopAssignments
                  .updatedWith(foundWorkshopId)(maybeStudents => Some(maybeStudents.getOrElse(Set.empty) + studentId))
                val updatedUnassignedTimeSlots = unassignedTimeSlots - foundTimeSlot
                val updatedStudents =
                  if (updatedUnassignedTimeSlots.isEmpty)
                    nextStudents // if a student has an assignment for each timeslot, no further distribution is needed
                  else {
                    val (_, updatedTopicSelections) = orderedTopicSelections.span(_.selectionPriority.prio <= prio)
                    val updatedAssignedTopics = assignedTopics + foundTopicId
                    val updatedStudent = headStudent.copy(
                      algoPrio = algoPrio + WorstPrio.prio - prio,
                      orderedTopicSelections = updatedTopicSelections,
                      unassignedTimeSlots = updatedUnassignedTimeSlots,
                      assignedTopics = updatedAssignedTopics,
                    )
                    // The student goes back in the list to a new place. The following usage of span is about 3 times
                    // faster than a solution with sortBy.
                    // nextStudents :+ updatedStudent.sortBy(s => (s.algoPrio, s.sortingOrder))
                    val (lesserAlgoPrio, sameOrGreaterAlgoPrio) = nextStudents.span(_.algoPrio < updatedStudent.algoPrio)
                    val (sameAlgoPrio, greaterAlgoPrio) = sameOrGreaterAlgoPrio.span(_.algoPrio == updatedStudent.algoPrio)
                    val (lesserSortingOrder, greaterSortingOrder) = sameAlgoPrio.span(_.sortingOrder < updatedStudent.sortingOrder)
                    (lesserAlgoPrio ++ lesserSortingOrder :+ updatedStudent) ++ greaterSortingOrder ++ greaterAlgoPrio
                  }
                recursion12(findWorkshopId)(updatedWorkshopAssignments, accUndistributableStudents, updatedStudents)
            }
        }

      val maybeDistribution1 = maybeDistribution0.flatMap { case (workshopAssignmentsSoFar, notYetDistributedStudents) =>
        recursion12(findWorkshopId1)(
          accWorkshopAssignments = workshopAssignmentsSoFar,
          accUndistributableStudents = List.empty,
          remainingStudentsToDistribute = notYetDistributedStudents,
        )
      }
      logger.debug(s"maybeDistribution1: $maybeDistribution1")

      // Second or third round of distribution: For each student, select the next workshop which fulfils all mandatory
      // criteria and the given function isAssignable, regardless of the student's selection.
      // However, any student that needs to go through the second or third round has depleted her selections;
      // thus only normal workshops can be selected, i.e. which do not have the flag "onlyVoluntary".
      def findWorkshopId23(isAssignable: Set[TopicId] => Boolean): FindWorkshopId =
        (student: Student, workshopAssignments: WorkshopAssignments) =>
          normalWorkshops.collectFirst {
            case Workshop(workshopId, topicId, timeSlot, grades, seats)
              if student.unassignedTimeSlots.contains(timeSlot)
                && !student.assignedTopics.contains(topicId)
                && grades.contains(student.grade)
                && workshopAssignments.getOrElse(workshopId, Set.empty).size < seats.n
                && isAssignable(student.assignedTopics + topicId) =>
              logger.trace(s"found23: $workshopId at $timeSlot for $student.")
              (workshopId, topicId, SelectionPriority(Int.MaxValue), timeSlot)
          }

      // Second round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection, but still with max varying categories.
      def findWorkshopId2: FindWorkshopId = findWorkshopId23(haveMaxVaryingCategories)

      // Third round of distribution: For each student, select the next workshop which fulfils all criteria, regardless
      // of her selection, and only with min varying categories.
      def findWorkshopId3: FindWorkshopId = findWorkshopId23(haveMinVaryingCategories)

      val maybeDistribution2 = maybeDistribution1.flatMap { case (workshopAssignmentsSoFar, notYetDistributedStudents) =>
        recursion12(findWorkshopId2)(
          accWorkshopAssignments = workshopAssignmentsSoFar,
          accUndistributableStudents = List.empty,
          remainingStudentsToDistribute = notYetDistributedStudents,
        )
      }
      logger.debug(s"maybeDistribution2: $maybeDistribution2")

      // Third round of distribution: For each student, select the next workshop which fulfils nearly all criteria,
      // regardless of her selection. The criteria that no 3 workshops of category sports shall be assigned is removed.
      // If no workshop can be found, the distribution fails.
      @tailrec
      def recursion3(accWorkshopAssignments: WorkshopAssignments, remainingStudentsToDistribute: List[Student]): Option[WorkshopAssignments] =
        remainingStudentsToDistribute match {
          case Nil =>
            logger.debug("Successful end of recursion3.")
            Some(accWorkshopAssignments)
          case ::(headStudent@Student(_, _, studentId, _, _, unassignedTimeSlots, assignedTopics), nextStudents) =>
            findWorkshopId3(headStudent, accWorkshopAssignments) match {
              case None =>
                logger.debug(s"Unsuccessful end of recursion3. No suitable workshop found for student $headStudent.")
                None
              case Some((foundWorkshopId, foundTopicId, _, foundTimeSlot)) =>
                val updatedWorkshopAssignments = accWorkshopAssignments
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

      // Make sure each workshop has a set of students. If not yet the case, add an empty set.
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
  // It is easier if we have our own data type. Ordering makes most sense by selection priority;
  // thus we use the flipped order of values compared to SelectedTopics.
  private final case class TopicSelection(selectionPriority: SelectionPriority, topicId: TopicId)

  private final case class Student(
                                    algoPrio: Int,
                                    sortingOrder: Int,
                                    studentId: StudentId,
                                    grade: Grade,
                                    orderedTopicSelections: List[TopicSelection],
                                    unassignedTimeSlots: Set[TimeSlot],
                                    assignedTopics: Set[TopicId],
                                  )

  private type DistributeFromPreOrdered = (DistributeWorkshopFilling, Topics, List[Workshop], List[Student]) => Option[WorkshopAssignments]
  private type FindWorkshopId = (Student, WorkshopAssignments) => Option[(WorkshopId, TopicId, SelectionPriority, TimeSlot)]

  // See https://github.com/scala/bug/issues/6675 and https://github.com/scala/bug/issues/6111
  // for the need for a holder to avoid deprecation message on (scala/bug#6675)
  private case class Holder[T](_1: T) extends Product1[T]

}
