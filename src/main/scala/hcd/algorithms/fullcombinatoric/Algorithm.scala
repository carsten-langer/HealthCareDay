package hcd.algorithms.fullcombinatoric

import com.typesafe.scalalogging.StrictLogging
import hcd.model.Metric._
import hcd.model._
import io.cvbio.collection.mutable.bimap.BiMap

object Algorithm extends StrictLogging {

  /** This algorithm's distribution function. */
  def distributionAlgorithm: DistributionAlgorithm =
    (topics: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) =>
      distributeStudentsToWorkshops(comboSize = 3)(topics, workshops)(studentsSelectedTopics) map {
        case (workshopAssignments, _, _) => workshopAssignments
      }

  // If a selected workshop topic is not contained in the concrete workshops, it is ignored.
  // Only workshops eligible for the grade are selected.
  protected[algorithms] def matchingWorkshopsFromSelectedTopics(workshops: Workshops)(grade: Grade, selectedTopics: SelectedTopics): MatchingWorkshops =
    selectedTopics
      .toMap // transform back from BiMap to Map, so that several workshops can have the same selection priority
      .flatMap { case (topicId, selectionPriority) =>
        workshops.collect { case (workshopId, (`topicId`, _, grades, _)) if grades.contains(grade) =>
          workshopId -> selectionPriority
        }
      }

  // If a selected workshop topic is not contained in the concrete workshops, it is ignored.
  protected[algorithms] def studentsMatchingWorkshopsFromStudentSelectedTopics(workshops: Workshops)(studentsSelectedTopics: StudentsSelectedTopics): StudentsMatchingWorkshops =
    studentsSelectedTopics
      .view
      .mapValues((matchingWorkshopsFromSelectedTopics(workshops)(_, _)).tupled)
      .toMap

  private def extract[A](extractor: WorkshopCandidate => A): WorkshopComboCandidate => Iterable[A] = workshopComboCandidate =>
    workshopComboCandidate
      .values // yields a Set of workshop candidates
      .toSeq // transform to Seq to allow several workshop candidates to have the same A being extracted
      .map(extractor)

  // empty iterable => false
  private def areDistinct[A](it: Iterable[A]): Boolean = it.nonEmpty && it.groupBy(identity).values.forall(_.size == 1)

  // empty workshop combo candidate => false
  protected[algorithms] def hasDistinctTopicIds: WorkshopComboCandidate => Boolean = extract(_.topicId).andThen(areDistinct)

  // empty workshop combo candidate => false
  protected[algorithms] def hasDistinctTimeslots: WorkshopComboCandidate => Boolean = extract(_.timeSlot).andThen(areDistinct)

  // Students are not allowed to get assigned a combo with all workshops of category nutrition,
  // nor a combo with all workshops of category relaxation.
  // They are allowed to get assigned a combo with all workshops of category other or sports, though.
  // empty workshop combo candidate => false
  protected[algorithms] def hasVaryingCategories: WorkshopComboCandidate => Boolean = workshopComboCandidate => {
    val categories = extract(_.category)(workshopComboCandidate)
    categories.exists(_ != Nutrition) && categories.exists(_ != Relaxation)
  }

  // Ideally, students should get assigned a combo which contains at least 1 workshop with selection priority <= 3.
  // If this filter is taken as hard boundary, it will reduce a lot the number of combo candidates,
  // but there may be the risk that no distribution could be found.
  // empty workshop combo candidate => false
  protected[algorithms] def hasSufficientSelectionPriority: WorkshopComboCandidate => Boolean = workshopComboCandidate =>
    workshopComboCandidate.nonEmpty &&
      workshopComboCandidate
        .values
        .map { case WorkshopCandidate(_, _, _, SelectionPriority(prio)) => prio }
        .min <= 3

  /**
   * From all matching workshops, generate all combinations of comboSize workshops which are possible.
   * A combo of workshops is possible if the workshops in that combo do not have the same timeslot nor the same
   * topic id and where the combo also complies to a given extra filter.
   *
   * @param comboSize             Number of workshops in a combination, e.g. 3.
   * @param extraFilterPredicates Select a candidate of a workshop combination only if all extraFilterPredicates are
   *                              true for it.
   */
  protected[algorithms] def generateWorkshopCombos(workshops: Workshops, topics: Topics, comboSize: Int, extraFilterPredicates: WorkshopComboCandidate => Boolean*)(matchingWorkshops: MatchingWorkshops): Set[WorkshopCombo] = {
    val extraFilterPredicate: WorkshopComboCandidate => Boolean = workshopComboCandidate =>
      extraFilterPredicates.foldLeft(true) { case (result, predicate) => result && predicate(workshopComboCandidate) }
    matchingWorkshops
      .map { case (workshopId, selectionPriority) =>
        val (topicId, timeSlot, _, _) = workshops(workshopId)
        val category = topics(topicId)
        workshopId -> WorkshopCandidate(topicId, timeSlot, category, selectionPriority)
      }
      .toSeq
      .combinations(comboSize)
      .map(BiMap.from)
      .toSet
      .filter(hasDistinctTopicIds)
      .filter(hasDistinctTimeslots)
      .filter(extraFilterPredicate)
      .map(workshopComboCandidate =>
        workshopComboCandidate.map { case (workshopId, WorkshopCandidate(_, _, category, selectionPriority)) =>
          workshopId -> PossibleWorkshop(category, selectionPriority)
        }
      )
  }

  protected[algorithms] def generateStudentsWorkshopCombos(workshops: Workshops, topics: Topics, comboSize: Int)(studentsSelectedTopics: StudentsSelectedTopics): Map[StudentId, Set[WorkshopCombo]] =
    studentsMatchingWorkshopsFromStudentSelectedTopics(workshops)(studentsSelectedTopics)
      .view
      .mapValues(generateWorkshopCombos(workshops, topics, comboSize, hasVaryingCategories, hasSufficientSelectionPriority))
      .toMap

  protected[algorithms] def addMetricsToStudentsWorkshopCombos(studentsWorkshopCombos: Map[StudentId, Set[WorkshopCombo]]): Map[StudentId, Set[(Set[WorkshopId], Metric)]] =
    studentsWorkshopCombos
      .view
      .mapValues(workshopCombos =>
        workshopCombos.map { workshopCombo =>
          val prios = workshopCombo.map { case (_, PossibleWorkshop(_, selectionPriority)) => selectionPriority }
          val categories = workshopCombo.map { case (_, PossibleWorkshop(category, _)) => category }
          val metric = add(metricFromSelectionPriorities(prios), metricFromCategories(categories))
          // BiMap.keySet would return a collection.Set, but we require a collection.immutable.Set, which Map.keySet
          // provides, thus transform the BiMap back to a Map
          val workshopIds = workshopCombo.toMap.keySet
          workshopIds -> metric
        }
      )
      .toMap

  // Orders students and workshop combos, which is needed to yield a stable distribution so that during the unit tests
  // the expected outcome can be pre-calculated.
  // Also leaves the data structures as lists, as during the recursion we prefer these data types which,
  // even if they are less expressive, are slightly faster.
  private def orderStudentsWorkshopCombosWithMetrics(studentsWorkshopCombosWithMetrics: Map[StudentId, Set[(Set[WorkshopId], Metric)]]): List[(StudentId, List[(List[WorkshopId], Metric)])] = {
    import Ordering.Implicits._
    studentsWorkshopCombosWithMetrics
      .view
      .mapValues(workshopCombos =>
        workshopCombos
          .toList
          .map { case (workshopCombo, metric) =>
            val newWorkshopCombo = workshopCombo
              .toList
              // Sort workshops within a single combo according to workshop id.
              // This is a prerequisite to find the order between workshop combos.
              .sortBy(_.id)
            (newWorkshopCombo, metric)
          }
          // Sort workshop combos by a) the metric and b) the already ordered list of workshop ids where each list
          // represents one workshop combo which was already ordered by workshop id.
          .sortBy { case (workshopCombo, Metric(m)) =>
            val workshopIds = workshopCombo.map(_.id)
            (m, workshopIds)
          }
      )
      .toList
      // sort by students
      .sortBy { case (StudentId(id), _) => id }
  }

  // A student with an empty list of possible workshop combos would only happen if a student has made a selection of
  // workshop topics such that no combinations of workshops are possible.
  // During production, there should be a test upfront to not even start the distribution in such case.
  // During development with arbitrary random data this situation can happen, as the random input data could
  // be such that the hasVaryingCategories filter filters out all conflicting workshop combos and leaves no
  // workshop combos. In this case, remove the student from the list of students so that the following recursion does
  // not need to handle this situation which would not occur in production.
  // Also print out the removed students and the total number of left over students.
  private def removeStudentsWithoutWorkshopCombos(orderedStudentsWorkshopCombosWithMetrics: List[(StudentId, List[(List[WorkshopId], Metric)])]): List[(StudentId, List[(List[WorkshopId], Metric)])] = {
    val filteredStudents = orderedStudentsWorkshopCombosWithMetrics.filter {
      case (studentId, Nil) =>
        logger.error(s"$studentId has no possible workshop combos, removing this student!")
        false
      case _ => true
    }
    logger.debug(s"${filteredStudents.size} students forwarded to recursion.")
    val lastStudents = 5
    val averageNoCombosLast10Students = filteredStudents
      .takeRight(lastStudents)
      .map { case (_, workshopCombos) => workshopCombos.size }.sum / lastStudents
    logger.debug(s"average number of workshop combos per student for the last $lastStudents students: $averageNoCombosLast10Students")
    filteredStudents
  }

  /**
   * Checks if the given free workshop seats could still take on the workshopCombo.
   * If so, return a Some of the new free workshop seats, else return a None.
   */
  protected[algorithms] def checkAndUpdateFreeWorkshopSeats(freeWorkshopSeats: WorkshopSeats, workshopIds: Seq[WorkshopId]): Option[WorkshopSeats] = {
    val areEnoughFreeSeats = workshopIds.map(freeWorkshopSeats).forall(seats => seats.n > 0)
    Option.when(areEnoughFreeSeats)(
      workshopIds.foldLeft(freeWorkshopSeats) { case (accFreeWorkshopSeats, workshopId) =>
        val newFreeSeats = Seats(accFreeWorkshopSeats(workshopId).n - 1)
        accFreeWorkshopSeats.updated(workshopId, newFreeSeats)
      }
    )
  }

  protected[algorithms] def distributeStudentsToWorkshops(comboSize: Int)(topics: Topics, workshops: Workshops)(studentsSelectedTopics: StudentsSelectedTopics): Option[(WorkshopAssignments, Metric, WorkshopSeats)] = {
    val initialFreeWorkshopSeats = workshops.view.mapValues { case (_, _, _, seats) => seats }.toMap
    val studentsWorkshopCombos = generateStudentsWorkshopCombos(workshops, topics, comboSize)(studentsSelectedTopics)
    val studentsWorkshopCombosWithMetrics = addMetricsToStudentsWorkshopCombos(studentsWorkshopCombos)
    val orderedStudentsWorkshopCombosWithMetrics = orderStudentsWorkshopCombosWithMetrics(studentsWorkshopCombosWithMetrics)
    val orderedStudentsNonEmptyWorkshopCombosWithMetrics = removeStudentsWithoutWorkshopCombos(orderedStudentsWorkshopCombosWithMetrics)
    logger.debug(s"ordered and filtered input, first 2 students: ${orderedStudentsNonEmptyWorkshopCombosWithMetrics.take(2)}")

    val counterLogger = new CounterLogger(s => logger.debug(s)) // eta expansion not possible for macros

    type DistributedStudentsWorkshopCombos = List[(StudentId, Seq[WorkshopId])]

    // not tail-recursive, as per student the algorithm collects the results for all workshop combos and then
    // selects those which have the overall minimum metric
    // Returns a Some if a combination was found, else a None.
    def recursion(distributedStudentsWorkshopCombos: DistributedStudentsWorkshopCombos,
                  accMetric: Metric,
                  freeWorkshopSeats: WorkshopSeats,
                  studentsWorkshopCombosToDistribute: List[(StudentId, List[(List[WorkshopId], Metric)])],
                 ): Option[(DistributedStudentsWorkshopCombos, Metric, WorkshopSeats)] =
      studentsWorkshopCombosToDistribute match {
        case Nil => Some((distributedStudentsWorkshopCombos, accMetric, freeWorkshopSeats))
        case ::((studentId, Nil), _) =>
          // The situation that a student has an empty list of possible workshop combos should have been resolved
          // before entering the recursion.
          throw new IllegalStateException(s"$studentId has no workshop combos, such situation should have been dealt with before the recursion!")
        case ::((studentId, workshopCombosWithMetric), nextStudents) =>
          val possibleStudentsWorkshopCombosToDistributeFurther = workshopCombosWithMetric
            .map { case (workshopCombo, metric) =>
              logger.whenDebugEnabled {
                counterLogger.countAndLog(studentId, workshopCombo)
              }
              (workshopCombo, metric, checkAndUpdateFreeWorkshopSeats(freeWorkshopSeats, workshopCombo))
            }
            .collect { case (workshopCombo, metric, Some(newFreeWorkshopSeats)) =>
              val newDistributedStudentsWorkshopCombos = distributedStudentsWorkshopCombos.prepended((studentId, workshopCombo))
              val newMetric = add(accMetric, metric)
              (newDistributedStudentsWorkshopCombos, newMetric, newFreeWorkshopSeats)
            }
          // A List.map would materialize all entries, thus call the function (the recursion) on all entries.
          // But an Iterator.map only wraps the original Iterator and calls the function (the recursion) on each call
          // of it.next. As find uses the iterator, we can avoid calling recursion unnecessarily by first
          // converting the List to an Iterator.
          //logger.trace(s"before: $possibleStudentsWorkshopCombosToDistributeFurther")
          val maybeFirstResult = possibleStudentsWorkshopCombosToDistributeFurther
            .iterator
            .map { case (newDistributedStudentsWorkshopCombos, newMetric, newFreeWorkshopSeats) =>
              //logger.trace(s"inside recursion: $newDistributedStudentsWorkshopCombos")
              recursion(newDistributedStudentsWorkshopCombos, newMetric, newFreeWorkshopSeats, nextStudents)
            }
            .find(maybeResult => maybeResult.isDefined)
            .flatten
          //logger.trace(s"after: $maybeFirstResult")
          maybeFirstResult
      }

    val maybeResult = recursion(List.empty, initialMetric, initialFreeWorkshopSeats, orderedStudentsNonEmptyWorkshopCombosWithMetrics)
    maybeResult map {
      case (distributedStudentsWorkshopCombos, metric, leftFreeSeats) =>
        val emptyWorkshopAssignments = workshops.view.mapValues(_ => Set.empty[StudentId]).toMap
        val workshopAssignments = distributedStudentsWorkshopCombos.foldLeft(emptyWorkshopAssignments) {
          case (accWorkshopAssignments1, (studentId, workshopIds)) =>
            workshopIds.foldLeft(accWorkshopAssignments1) { case (accWorkshopAssignments2, workshopId) =>
              val newStudents = accWorkshopAssignments2(workshopId).incl(studentId)
              accWorkshopAssignments2.updated(workshopId, newStudents)
            }
        }
        (workshopAssignments, metric, leftFreeSeats)
    }
  }

}
