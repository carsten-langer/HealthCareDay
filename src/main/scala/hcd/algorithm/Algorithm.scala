package hcd.algorithm

import hcd.models._

object Algorithm {

  // If a selection is not contained in the workshops, it is ignored.
  protected[algorithm] def selectedWorkshopsFromWorkshopSelection(workshops: Workshops)(workshopSelection: WorkshopSelection): SelectedWorkshops =
    workshopSelection
      .toMap
      .flatMap { case (selectionPriority, choiceId) =>
        workshops.collect { case (workshopId, Workshop(category, `choiceId`, timeSlot, _)) =>
          (workshopId, SelectedWorkshop(category, choiceId, timeSlot, selectionPriority))
        }
      }

  // If a selection is not contained in the workshops, it is ignored.
  protected[algorithm] def studentsSelectedWorkshopsFromStudentWorkshopSelections(workshops: Workshops)(studentWorkshopSelections: StudentWorkshopSelections): Map[StudentId, SelectedWorkshops] =
    studentWorkshopSelections
      .view
      .mapValues(selectedWorkshopsFromWorkshopSelection(workshops))
      .toMap

  // empty selected workshops => false
  private def haveDistinct(extractor: SelectedWorkshop => Any)(selectedWorkshops: SelectedWorkshops): Boolean =
    selectedWorkshops.nonEmpty &&
      selectedWorkshops
        .values
        .groupBy(extractor)
        .values
        .forall(_.size == 1)

  // empty selected workshops => false
  protected[algorithm] def haveDistinctChoiceIds: SelectedWorkshops => Boolean = haveDistinct(_.choiceId)

  // empty selected workshops => false
  protected[algorithm] def haveDistinctTimeslots: SelectedWorkshops => Boolean = haveDistinct(_.timeSlot)

  // Students are not allowed to get assigned all workshops of category health, nor all workshops of category
  // relaxation. They are allowed to get assigned all workshops of category sports, though.
  // empty selected workshops => false
  protected[algorithm] def haveVaryingCategories: SelectedWorkshops => Boolean = selectedWorkshops =>
    selectedWorkshops.values.exists(_.category != Health) && selectedWorkshops.values.exists(_.category != Relaxation)

  protected[algorithm] def possibleWorkshopCombinations(n: Int, extraFilterPredicate: SelectedWorkshops => Boolean)(selectedWorkshops: SelectedWorkshops): Set[PossibleWorkshops] =
    selectedWorkshops
      .toSeq
      .combinations(n)
      .map(_.toMap)
      .filter(haveDistinctChoiceIds)
      .filter(haveDistinctTimeslots)
      .filter(extraFilterPredicate)
      .map(_.view.mapValues { case SelectedWorkshop(category, _, _, selectionPriority) =>
        PossibleWorkshop(category, selectionPriority)
      }.toMap)
      .toSet // adding here and not after .combinations, as else we would need to give specific type, else .toMap complains

  protected[algorithm] def possibleWorkshopCombinations(workshops: Workshops, n: Int)(studentWorkshopSelections: StudentWorkshopSelections): Map[StudentId, Set[PossibleWorkshops]] =
    studentsSelectedWorkshopsFromStudentWorkshopSelections(workshops)(studentWorkshopSelections)
      .view
      .mapValues(possibleWorkshopCombinations(n, haveVaryingCategories))
      .toMap

  protected[algorithm] def distributeStudentsToWorkshops(workshops: Workshops)(studentPossibleWorkshops: Map[StudentId, Set[PossibleWorkshops]]): (WorkshopAssignments, Int) = {
    case class FilledWorkshop(freeSeats: Int, students: Set[StudentId])
    type FilledWorkshops = Map[WorkshopId, FilledWorkshop]

    val initialFilledWorkshops = workshops.view.mapValues(workshop => FilledWorkshop(workshop.seats, Set.empty)).toMap
    import Ordering.Implicits._
    val orderedStudentPossibleWorkshops = studentPossibleWorkshops
      .view
      .mapValues(_
        .toList
        .map(_
          .toList
          .sortBy(_._2.selectionPriority.priority)
        )
        .sortBy(_.take(3).map(_._1.id))
      )
      .toList
      .sortBy(_._1.id)
    println(s"ordered input: $orderedStudentPossibleWorkshops")

    var currentN = 0
    val startTime = System.currentTimeMillis()

    // currently takes ca. 37 s to calculate 1 combination for Student 411
    // i.e. try all combos for students 412, 413, ..., 999
    def countAndPrint(studentId: StudentId, possibleWorkshops: List[(WorkshopId, PossibleWorkshop)]): Unit = {
      currentN += 1
      val now = System.currentTimeMillis()
      if (studentId.id < 412) {
        println(s"seconds spent: ${(now - startTime) / 1000}, currentN: $currentN, studentId: $studentId, possibleWorkshops: $possibleWorkshops")
      }
    }

    def fillWorkshopsAndCalculateMetric(filledWorkshops: FilledWorkshops, studentId: StudentId, possibleWorkshops: List[(WorkshopId, PossibleWorkshop)]): (FilledWorkshops, Int) = {
      countAndPrint(studentId, possibleWorkshops)
      val newFilledWorkshops = possibleWorkshops.foldLeft(filledWorkshops) {
        case (accFilledWorkshops, (workshopId, _)) =>
          accFilledWorkshops.updatedWith(workshopId)(_.map(filledWorkshops =>
            filledWorkshops.copy(
              freeSeats = filledWorkshops.freeSeats - 1,
              students = filledWorkshops.students.incl(studentId)
            )
          ))
      }
      val metric = possibleWorkshops.map(_._2.selectionPriority.priority).sum
      (newFilledWorkshops, metric)
    }

    // not tail-recursive, as per student the algorithm collects the results for all possible workshops and then
    // selects those which have the overall minimum metric
    def recursion(accFilledWorkshops: Map[WorkshopId, FilledWorkshop], accMetric: Int, studentsToDistribute: List[(StudentId, List[List[(WorkshopId, PossibleWorkshop)]])]): (WorkshopAssignments, Int) =
      studentsToDistribute match {
        case Nil => (accFilledWorkshops.view.mapValues(_.students).toMap, accMetric)
        case (_, Nil) :: _ =>
          // A student with an empty list of possible workshops would only happen if a student has made a selection of
          // workshop choices such that no combinations of workshops are possible.
          // During production, there should be a test upfront to not even start the distribution in such case.
          // During development with arbitrary random data this situation can happen, as the random input data could
          // be such that the haveVaryingCategories filter filters out all conflicting workshop combinations and leaves
          // no possible workshop combinations. In this case, continue without adding this student to any workshop.
          (accFilledWorkshops.view.mapValues(_.students).toMap, accMetric)
        case (student, possibleWorkshopsList) :: tailStudents =>
          val resultsThisStudent = possibleWorkshopsList.map { possibleWorkshops =>
            val (newAccFilledWorkshops, metricOfThisPossibleWorkshops) = fillWorkshopsAndCalculateMetric(accFilledWorkshops, student, possibleWorkshops)
            recursion(newAccFilledWorkshops, accMetric + metricOfThisPossibleWorkshops, tailStudents)
          }
          resultsThisStudent.minBy(_._2) // works as possibleWorkshopsList will not be Nil
      }

    recursion(initialFilledWorkshops, 0, orderedStudentPossibleWorkshops)
  }

}
