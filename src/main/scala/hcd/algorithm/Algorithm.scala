package hcd.algorithm

import hcd.models._
import io.cvbio.collection.mutable.bimap.BiMap

import scala.annotation.unused

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

  protected[algorithm] def distributeStudentsToWorkshops(workshops: Workshops, topics: Topics, @unused workshopSeats: WorkshopSeats, comboSize: Int)(studentsSelectedTopics: StudentsSelectedTopics): (WorkshopAssignments, Metric) = {
    val studentsWorkshopCombos = generateStudentsWorkshopCombos(workshops, topics, comboSize)(studentsSelectedTopics)
    val studentsWorkshopCombosWithMetrics = studentsWorkshopCombos
      .view
      .mapValues(workshopCombos =>
        workshopCombos.map { workshopCombo =>
          val prioMetric = workshopCombo.map { case (_, PossibleWorkshop(_, SelectionPriority(prio))) => prio }.sum
          val onlySports = workshopCombo.map { case (_, PossibleWorkshop(category, _)) => category }.forall(_ == Sports)
          val metric = Metric(prioMetric + (if (onlySports) 1000 else 0)) // malus if a combo contains only sports category
          workshopCombo -> metric
        }
      )

    // Orders students and workshop combos, which is needed to yield a stable distribution so that during the unit tests
    // the expected outcome can be pre-calculated.
    // Also leaves the data structures as lists, as during the recursion we prefer these data types which,
    // even if they are less expressive, are slightly faster.
    import Ordering.Implicits._
    val orderedStudentsWorkshopCombosWithMetrics: List[(StudentId, List[(List[(WorkshopId, PossibleWorkshop)], Metric)])] =
      studentsWorkshopCombosWithMetrics
        .mapValues(workshopCombos =>
          workshopCombos
            .toList
            .map { case (workshopCombo, metric) =>
              val newWorkshopCombo = workshopCombo
                .toList
                // Sort workshops within a single combo according to selection priority.
                // This is a prerequisite to find the order between workshop combos.
                .sortBy { case (_, PossibleWorkshop(_, SelectionPriority(prio))) => prio }
              (newWorkshopCombo, metric)
            }
            // Sort workshop combos by a) the metric and b) the already ordered list of workshop ids where each list
            // represents one workshop combo which was already ordered by selection priority.
            .sortBy { case (workshopCombo, Metric(metric)) =>
              val workshopIds = workshopCombo.map { case (WorkshopId(id), _) => id }
              (metric, workshopIds)
            }
        )
        .toList
        // sort by students
        .sortBy { case (StudentId(id), _) => id }
    println(s"ordered input, first 2 students: ${orderedStudentsWorkshopCombosWithMetrics.take(2)}")

    var currentN = 0
    val startTime = System.currentTimeMillis()

    // currently takes ca. 6 s to calculate 1 combination for Student 410
    // i.e. try all combos for students 411, 412, ..., 999
    // with 90 workshop combos per student
    def countAndPrint(studentId: StudentId, workshopCombo: List[(WorkshopId, PossibleWorkshop)]): Unit = {
      currentN += 1
      if (studentId.id < 411) {
        val now = System.currentTimeMillis()
        println(s"seconds spent: ${(now - startTime) / 1000}, currentN: $currentN, studentId: $studentId, workshopCombo: $workshopCombo")
      }
    }

    type DistributedStudentsWorkshopCombos = List[(StudentId, Seq[WorkshopId])]

    // not tail-recursive, as per student the algorithm collects the results for all workshop combos and then
    // selects those which have the overall minimum metric
    def recursion(distributedStudentsWorkshopCombos: DistributedStudentsWorkshopCombos, accMetric: Metric, studentsWorkshopCombosToDistribute: List[(StudentId, List[(List[(WorkshopId, PossibleWorkshop)], Metric)])]): (DistributedStudentsWorkshopCombos, Metric) =
      studentsWorkshopCombosToDistribute match {
        case Nil => (distributedStudentsWorkshopCombos, accMetric)
        case (_, Nil) :: _ =>
          // A student with an empty list of possible workshop combos would only happen if a student has made a
          // selection of workshop topics such that no combinations of workshops are possible.
          // During production, there should be a test upfront to not even start the distribution in such case.
          // During development with arbitrary random data this situation can happen, as the random input data could
          // be such that the hasVaryingCategories filter filters out all conflicting workshop combos and leaves no
          // possible workshop combos. In this case, continue without adding this student to any workshop.
          (distributedStudentsWorkshopCombos, accMetric)
        case (studentId, workshopCombos) :: tailStudents =>
          val resultsThisStudent = workshopCombos.map { case (workshopCombo, Metric(metric)) =>
            countAndPrint(studentId, workshopCombo)
            val workshopIds = workshopCombo.map { case (id, _) => id }
            val newDistributedStudentsWorkshopCombos = distributedStudentsWorkshopCombos.prepended((studentId, workshopIds))
            val newMetric = Metric(accMetric.metric + metric)
            recursion(newDistributedStudentsWorkshopCombos, newMetric, tailStudents)
          }
          resultsThisStudent.minBy { case (_, Metric(metric)) => metric } // works as workshopCombos will not be Nil
      }

    val (distributedStudentsWorkshopCombos, metric) = recursion(List.empty, Metric(0), orderedStudentsWorkshopCombosWithMetrics)

    val emptyWorkshopAssignments = workshops.view.mapValues(_ => Set.empty[StudentId]).toMap
    val workshopAssignments = distributedStudentsWorkshopCombos.foldLeft(emptyWorkshopAssignments) { case (accMap1, (studentId, workshopIds)) =>
      workshopIds.foldLeft(accMap1) { case (accMap2, workshopId) =>
        accMap2.updatedWith(workshopId)(_.map(students => students.incl(studentId)))
      }
    }
    (workshopAssignments, metric)
  }

}
