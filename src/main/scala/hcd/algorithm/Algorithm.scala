package hcd.algorithm

import hcd.models._

object Algorithm {

  // If a selected workshop choice is not contained in the workshops, it is ignored.
  protected[algorithm] def matchingWorkshopsFromSelectedWorkshopChoice(workshops: Workshops)(selectedWorkshopChoices: SelectedWorkshopChoices): MatchingWorkshops =
    selectedWorkshopChoices
      .toMap // transform back from BiMap to Map, so that several workshops can have the same selection priority
      .flatMap { case (selectionPriority, choiceId) =>
        workshops.collect { case (workshopId, Workshop(_, `choiceId`, _, _)) =>
          workshopId -> selectionPriority
        }
      }

  // If a selected workshop choice is not contained in the workshops, it is ignored.
  protected[algorithm] def studentsMatchingWorkshopsFromStudentSelectedWorkshopChoices(workshops: Workshops)(studentsSelectedWorkshopChoices: StudentsSelectedWorkshopChoices): StudentsMatchingWorkshops =
    studentsSelectedWorkshopChoices
      .view
      .mapValues(matchingWorkshopsFromSelectedWorkshopChoice(workshops))
      .toMap

  private def extract[A](extractor: Workshop => A): WorkshopComboCandidate => Iterable[A] = workshopComboCandidate =>
    workshopComboCandidate.values.map(possibleWorkshopCandidate => extractor(possibleWorkshopCandidate.workshop))

  // empty iterable => false
  private def areDistinct[A](it: Iterable[A]): Boolean = it.nonEmpty && it.groupBy(identity).values.forall(_.size == 1)

  // empty workshop combo candidate => false
  protected[algorithm] def hasDistinctChoiceIds: WorkshopComboCandidate => Boolean = extract(_.choiceId).andThen(areDistinct)

  // empty workshop combo candidate => false
  protected[algorithm] def hasDistinctTimeslots: WorkshopComboCandidate => Boolean = extract(_.timeSlot).andThen(areDistinct)

  // Students are not allowed to get assigned a combo with all workshops of category health,
  // nor a combo with all workshops of category relaxation.
  // They are allowed to get assigned a combo with all workshops of category sports, though.
  // empty workshop combo candidate => false
  protected[algorithm] def hasVaryingCategories: WorkshopComboCandidate => Boolean = workshopComboCandidate => {
    val categories = extract(_.category)(workshopComboCandidate)
    categories.exists(_ != Health) && categories.exists(_ != Relaxation)
  }

  /** From all matching workshops, generate all combinations of comboSize workshops which are possible.
   * A combo of workshops is possible if the workshops in that combo do not have the same timeslot nor the same
   * choice id and where the combo also complies to a given extra filter.
   *
   * @param comboSize            Number of workshops in a combination, e.g. 3.
   * @param extraFilterPredicate A candidate of a workshop combination is only selected if extraFilterPredicate is true
   *                             for it.
   */
  protected[algorithm] def generateWorkshopCombos(workshops: Workshops, comboSize: Int, extraFilterPredicate: WorkshopComboCandidate => Boolean)(matchingWorkshops: MatchingWorkshops): Set[WorkshopCombo] =
    matchingWorkshops
      .map { case (workshopId, selectionPriority) =>
        workshopId -> PossibleWorkshopCandidate(workshops(workshopId), selectionPriority)
      }
      .toSeq
      .combinations(comboSize)
      .map(_.toMap)
      .toSet
      .filter(hasDistinctChoiceIds)
      .filter(hasDistinctTimeslots)
      .filter(extraFilterPredicate)
      .map(workshopComboCandidate =>
        workshopComboCandidate.map { case (workshopId, PossibleWorkshopCandidate(workshop, selectionPriority)) =>
          workshopId -> PossibleWorkshop(workshop.category, selectionPriority)
        }
      )

  protected[algorithm] def generateStudentsWorkshopCombos(workshops: Workshops, comboSize: Int)(studentsSelectedWorkshopChoices: StudentsSelectedWorkshopChoices): Map[StudentId, Set[WorkshopCombo]] =
    studentsMatchingWorkshopsFromStudentSelectedWorkshopChoices(workshops)(studentsSelectedWorkshopChoices)
      .view
      .mapValues(generateWorkshopCombos(workshops, comboSize, hasVaryingCategories))
      .toMap

  protected[algorithm] def distributeStudentsToWorkshops(workshops: Workshops, comboSize: Int)(studentsSelectedWorkshopChoices: StudentsSelectedWorkshopChoices): (WorkshopAssignments, Metric) = {
    val studentsWorkshopCombos = generateStudentsWorkshopCombos(workshops, comboSize)(studentsSelectedWorkshopChoices)

    // Orders students and workshop combos, which is needed to yield a stable distribution so that during the unit tests
    // the expected outcome can be pre-calculated.
    // Also leaves the data structures as lists, as during the recursion we prefer these data types which,
    // even if they are less expressive, are slightly faster.
    import Ordering.Implicits._
    val orderedStudentsWorkshopCombos = studentsWorkshopCombos
      .view
      .mapValues(workshopCombos =>
        workshopCombos
          .toList
          .map(workshopCombo =>
            workshopCombo
              .toList
              // Sort workshops within a single combo according to selection priority.
              // This is a prerequisite to find the order between workshop combos.
              .sortBy { case (_, PossibleWorkshop(_, SelectionPriority(priority))) => priority }
          )
          // Sort workshop combos by the already ordered list of workshop ids where each list
          // represents one workshop combo which was already ordered by selection priority.
          .sortBy(workshopCombo => workshopCombo.map { case (WorkshopId(id), _) => id })
      )
      .toList
      // sort by students
      .sortBy { case (StudentId(id), _) => id }
    println(s"ordered input, first 2 students: ${orderedStudentsWorkshopCombos.take(2)}")

    case class FilledWorkshop(freeSeats: Int, students: Set[StudentId])
    type FilledWorkshops = Map[WorkshopId, FilledWorkshop]
    val initialFilledWorkshops = workshops.view.mapValues(workshop => FilledWorkshop(workshop.seats, Set.empty)).toMap

    var currentN = 0
    val startTime = System.currentTimeMillis()

    // currently takes ca. 37 s to calculate 1 combination for Student 411
    // i.e. try all combos for students 412, 413, ..., 999
    def countAndPrint(studentId: StudentId, workshopCombo: List[(WorkshopId, PossibleWorkshop)]): Unit = {
      currentN += 1
      val now = System.currentTimeMillis()
      if (studentId.id < 412) {
        println(s"seconds spent: ${(now - startTime) / 1000}, currentN: $currentN, studentId: $studentId, workshopCombo: $workshopCombo")
      }
    }

    def fillWorkshopsAndCalculateMetric(filledWorkshops: FilledWorkshops, studentId: StudentId, workshopCombo: List[(WorkshopId, PossibleWorkshop)]): (FilledWorkshops, Metric) = {
      countAndPrint(studentId, workshopCombo)
      val newFilledWorkshops = workshopCombo.foldLeft(filledWorkshops) {
        case (accFilledWorkshops, (workshopId, _)) =>
          accFilledWorkshops.updatedWith(workshopId)(_.map(filledWorkshop =>
            filledWorkshop.copy(
              freeSeats = filledWorkshop.freeSeats - 1,
              students = filledWorkshop.students.incl(studentId)
            )
          ))
      }
      val metric = workshopCombo.map { case (_, workshop) => workshop.selectionPriority.priority }.sum
      (newFilledWorkshops, Metric(metric))
    }

    // not tail-recursive, as per student the algorithm collects the results for all workshop combos and then
    // selects those which have the overall minimum metric
    def recursion(accFilledWorkshops: FilledWorkshops, accMetric: Metric, studentsWorkshopCombosToDistribute: List[(StudentId, List[List[(WorkshopId, PossibleWorkshop)]])]): (WorkshopAssignments, Metric) =
      studentsWorkshopCombosToDistribute match {
        case Nil => (accFilledWorkshops.view.mapValues(_.students).toMap, accMetric)
        case (_, Nil) :: _ =>
          // A student with an empty list of possible workshop combos would only happen if a student has made a
          // selection of workshop choices such that no combinations of workshops are possible.
          // During production, there should be a test upfront to not even start the distribution in such case.
          // During development with arbitrary random data this situation can happen, as the random input data could
          // be such that the hasVaryingCategories filter filters out all conflicting workshop combos and leaves no
          // possible workshop combos. In this case, continue without adding this student to any workshop.
          (accFilledWorkshops.view.mapValues(_.students).toMap, accMetric)
        case (student, workshopCombos) :: tailStudents =>
          val resultsThisStudent = workshopCombos.map { workshopCombo =>
            val (newAccFilledWorkshops, metricOfThisWorkshopCombo) = fillWorkshopsAndCalculateMetric(accFilledWorkshops, student, workshopCombo)
            recursion(newAccFilledWorkshops, Metric(accMetric.metric + metricOfThisWorkshopCombo.metric), tailStudents)
          }
          resultsThisStudent.minBy { case (_, metric) => metric.metric } // works as workshopCombos will not be Nil
      }

    recursion(initialFilledWorkshops, Metric(0), orderedStudentsWorkshopCombos)
  }

}
