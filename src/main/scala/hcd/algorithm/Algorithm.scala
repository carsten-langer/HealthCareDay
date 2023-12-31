package hcd.algorithm

import hcd.models._
import io.cvbio.collection.mutable.bimap.BiMap

object Algorithm {

  // If a selected workshop topic is not contained in the concrete workshops, it is ignored.
  protected[algorithm] def matchingWorkshopsFromSelectedTopics(workshops: Workshops)(selectedTopics: SelectedTopics): MatchingWorkshops =
    selectedTopics
      .toMap // transform back from BiMap to Map, so that several workshops can have the same selection priority
      .flatMap { case (topicId, selectionPriority) =>
        workshops.collect { case (workshopId, TopicTimeslot(`topicId`, _)) =>
          workshopId -> selectionPriority
        }
      }

  // If a selected workshop topic is not contained in the concrete workshops, it is ignored.
  protected[algorithm] def studentsMatchingWorkshopsFromStudentSelectedTopics(workshops: Workshops)(studentsSelectedTopics: StudentsSelectedTopics): StudentsMatchingWorkshops =
    studentsSelectedTopics
      .view
      .mapValues(matchingWorkshopsFromSelectedTopics(workshops))
      .toMap

  private def extract[A](extractor: WorkshopCandidate => A): WorkshopComboCandidate => Iterable[A] = workshopComboCandidate =>
    workshopComboCandidate
      .values // yields a Set of workshop candidates
      .toSeq // transform to Seq to allow several workshop candidates to have the same A being extracted
      .map(extractor)

  // empty iterable => false
  private def areDistinct[A](it: Iterable[A]): Boolean = it.nonEmpty && it.groupBy(identity).values.forall(_.size == 1)

  // empty workshop combo candidate => false
  protected[algorithm] def hasDistinctTopicIds: WorkshopComboCandidate => Boolean = extract(_.topicId).andThen(areDistinct)

  // empty workshop combo candidate => false
  protected[algorithm] def hasDistinctTimeslots: WorkshopComboCandidate => Boolean = extract(_.timeSlot).andThen(areDistinct)

  // Students are not allowed to get assigned a combo with all workshops of category nutrition,
  // nor a combo with all workshops of category relaxation.
  // They are allowed to get assigned a combo with all workshops of category sports, though.
  // empty workshop combo candidate => false
  protected[algorithm] def hasVaryingCategories: WorkshopComboCandidate => Boolean = workshopComboCandidate => {
    val categories = extract(_.category)(workshopComboCandidate)
    categories.exists(_ != Nutrition) && categories.exists(_ != Relaxation)
  }

  // Ideally, students should get assigned a combo which contains at least 1 workshop with selection priority <= 3.
  // If this filter is taken as hard boundary, it will reduce a lot the number of combo candidates,
  // but there may be the risk that no distribution could be found.
  // empty workshop combo candidate => false
  protected[algorithm] def hasSufficientSelectionPriority: WorkshopComboCandidate => Boolean = workshopComboCandidate =>
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
  protected[algorithm] def generateWorkshopCombos(workshops: Workshops, topics: Topics, comboSize: Int, extraFilterPredicates: WorkshopComboCandidate => Boolean*)(matchingWorkshops: MatchingWorkshops): Set[WorkshopCombo] = {
    val extraFilterPredicate: WorkshopComboCandidate => Boolean = workshopComboCandidate =>
      extraFilterPredicates.foldLeft(true) { case (result, predicate) => result && predicate(workshopComboCandidate) }
    matchingWorkshops
      .map { case (workshopId, selectionPriority) =>
        val TopicTimeslot(topicId, timeSlot) = workshops(workshopId)
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

  protected[algorithm] def generateStudentsWorkshopCombos(workshops: Workshops, topics: Topics, comboSize: Int)(studentsSelectedTopics: StudentsSelectedTopics): Map[StudentId, Set[WorkshopCombo]] =
    studentsMatchingWorkshopsFromStudentSelectedTopics(workshops)(studentsSelectedTopics)
      .view
      .mapValues(generateWorkshopCombos(workshops, topics, comboSize, hasVaryingCategories, hasSufficientSelectionPriority))
      .toMap

  /**
   * Checks if the given free workshop seats could still take on the workshopCombo.
   * If so, return a Some of the new free workshop seats, else return a None.
   */
  protected[algorithm] def checkAndUpdateFreeWorkshopSeats(freeWorkshopSeats: WorkshopSeats, workshopIds: Seq[WorkshopId]): Option[WorkshopSeats] = {
    val areEnoughFreeSeats = workshopIds.map(freeWorkshopSeats).forall(seats => seats.n > 0)
    Option.when(areEnoughFreeSeats)(
      workshopIds.foldLeft(freeWorkshopSeats) { case (accFreeWorkshopSeats, workshopId) =>
        val newFreeSeats = Seats(accFreeWorkshopSeats(workshopId).n - 1)
        accFreeWorkshopSeats.updated(workshopId, newFreeSeats)
      }
    )
  }

  protected[algorithm] def distributeStudentsToWorkshops(workshops: Workshops, topics: Topics, initialFreeWorkshopSeats: WorkshopSeats, comboSize: Int)(studentsSelectedTopics: StudentsSelectedTopics): Option[(WorkshopAssignments, Metric, WorkshopSeats)] = {
    val studentsWorkshopCombos = generateStudentsWorkshopCombos(workshops, topics, comboSize)(studentsSelectedTopics)
    val studentsWorkshopCombosWithMetrics = studentsWorkshopCombos
      .view
      .mapValues(workshopCombos =>
        workshopCombos.map { workshopCombo =>
          val prioMetric = workshopCombo.map { case (_, PossibleWorkshop(_, SelectionPriority(prio))) => prio }.sum
          val onlySports = workshopCombo.map { case (_, PossibleWorkshop(category, _)) => category }.forall(_ == Sports)
          val metric = Metric(prioMetric + (if (onlySports) 1000 else 0)) // malus if a combo contains only sports category
          val workshopIds = workshopCombo.keySet
          workshopIds -> metric
        }
      )

    // Orders students and workshop combos, which is needed to yield a stable distribution so that during the unit tests
    // the expected outcome can be pre-calculated.
    // Also leaves the data structures as lists, as during the recursion we prefer these data types which,
    // even if they are less expressive, are slightly faster.
    import Ordering.Implicits._
    val orderedStudentsWorkshopCombosWithMetrics: List[(StudentId, List[(List[WorkshopId], Metric)])] =
      studentsWorkshopCombosWithMetrics
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
            .sortBy { case (workshopCombo, Metric(metric)) =>
              val workshopIds = workshopCombo.map(_.id)
              (metric, workshopIds)
            }
        )
        .toList
        // sort by students
        .sortBy { case (StudentId(id), _) => id }
    println(s"ordered input, first 2 students: ${orderedStudentsWorkshopCombosWithMetrics.take(2)}")

    // A student with an empty list of possible workshop combos would only happen if a student has made a selection of
    // workshop topics such that no combinations of workshops are possible.
    // During production, there should be a test upfront to not even start the distribution in such case.
    // During development with arbitrary random data this situation can happen, as the random input data could
    // be such that the hasVaryingCategories filter filters out all conflicting workshop combos and leaves no
    // workshop combos. In this case, remove the student from the list of students so that the following recursion does
    // not need to handle this situation which would not occur in production.
    // Also print out the removed students and the total number of left over students.
    val orderedStudentsNonEmptyWorkshopCombosWithMetrics: List[(StudentId, List[(List[WorkshopId], Metric)])] = {
      val filteredStudents = orderedStudentsWorkshopCombosWithMetrics.filter {
        case (studentId, Nil) =>
          println(s"$studentId has no possible workshop combos, removing this student!")
          false
        case _ => true
      }
      val noStudents = math.max(1, filteredStudents.size)
      println(s"$noStudents students forwarded to recursion.")
      val lastStudents = 5
      val averageNoCombosLast10Students = filteredStudents
        .takeRight(lastStudents)
        .map { case (_, workshopCombos) => workshopCombos.size }.sum / lastStudents
      println(s"average number of workshop combos per student for the last $lastStudents students: $averageNoCombosLast10Students")
      filteredStudents
    }

    val counterPrinter = new CounterPrinter

    type DistributedStudentsWorkshopCombos = List[(StudentId, Seq[WorkshopId])]

    /**
     * @return A Some if a combination was found, else a None.
     */
    // not tail-recursive, as per student the algorithm collects the results for all workshop combos and then
    // selects those which have the overall minimum metric
    def recursion(distributedStudentsWorkshopCombos: DistributedStudentsWorkshopCombos,
                  accMetric: Metric,
                  freeWorkshopSeats: WorkshopSeats,
                  studentsWorkshopCombosToDistribute: List[(StudentId, List[(List[WorkshopId], Metric)])],
                 ): Option[(DistributedStudentsWorkshopCombos, Metric, WorkshopSeats)] =
      studentsWorkshopCombosToDistribute match {
        case Nil => Some((distributedStudentsWorkshopCombos, accMetric, freeWorkshopSeats))
        case (studentId, Nil) :: _ =>
          // The situation that a student has an empty list of possible workshop combos should have been resolved
          // before entering the recursion.
          throw new IllegalStateException(s"$studentId has no workshop combos, such situation should have been dealt with before the recursion!")
        case (studentId, workshopCombos) :: tailStudents =>
          // using List flatMap here ...
          val resultsThisStudent = workshopCombos.flatMap { case (workshopCombo, Metric(metric)) =>
            counterPrinter.countAndPrint(studentId, workshopCombo)
            val maybeNewFreeWorkshopSeats = checkAndUpdateFreeWorkshopSeats(freeWorkshopSeats, workshopCombo)
            val maybeResult = maybeNewFreeWorkshopSeats.flatMap { newFreeWorkshopSeats =>
              val newDistributedStudentsWorkshopCombos = distributedStudentsWorkshopCombos.prepended((studentId, workshopCombo))
              val newMetric = Metric(accMetric.metric + metric)
              recursion(newDistributedStudentsWorkshopCombos, newMetric, newFreeWorkshopSeats, tailStudents)
            }
            // and mapping an Option to a List ...
            maybeResult.map(List(_)).getOrElse(List.empty)
          }
          // and checking the list's emptiness is a poor man's "traverse" which converts a List[Option] to Option[List]
          if (resultsThisStudent.nonEmpty)
            Some(resultsThisStudent.minBy { case (_, Metric(metric), _) => metric })
          else
            None
      }

    val maybeResult = recursion(List.empty, Metric(0), initialFreeWorkshopSeats, orderedStudentsNonEmptyWorkshopCombosWithMetrics)
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
