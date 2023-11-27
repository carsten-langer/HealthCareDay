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
  private def haveDistinctField(extractor: SelectedWorkshop => Any)(selectedWorkshops: SelectedWorkshops): Boolean =
    selectedWorkshops.nonEmpty &&
      selectedWorkshops
        .values
        .groupBy(extractor)
        .values
        .forall(_.size == 1)

  // empty selected workshops => false
  protected[algorithm] def haveDistinctChoiceIds: SelectedWorkshops => Boolean = haveDistinctField(_.choiceId)

  // empty selected workshops => false
  protected[algorithm] def haveDistinctTimeslots: SelectedWorkshops => Boolean = haveDistinctField(_.timeSlot)

  protected[algorithm] def possibleWorkshopCombinations(n: Int)(selectedWorkshops: SelectedWorkshops): Set[PossibleWorkshops] =
    selectedWorkshops
      .toSeq
      .combinations(n)
      .map(_.toMap)
      .filter(haveDistinctChoiceIds)
      .filter(haveDistinctTimeslots)
      .map(_.view.mapValues { case SelectedWorkshop(category, _, _, selectionPriority) =>
        PossibleWorkshop(category, selectionPriority)
      }.toMap)
      .toSet // adding here and not after .combinations, as else we would need to give specific type, else .toMap complains

  protected[algorithm] def possibleWorkshopCombinations(workshops: Workshops, n: Int)(studentWorkshopSelections: StudentWorkshopSelections): Map[StudentId, Set[PossibleWorkshops]] =
    studentsSelectedWorkshopsFromStudentWorkshopSelections(workshops)(studentWorkshopSelections)
      .view
      .mapValues(possibleWorkshopCombinations(n))
      .toMap

}
