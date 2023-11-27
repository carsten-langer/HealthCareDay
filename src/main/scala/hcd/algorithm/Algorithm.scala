package hcd.algorithm

import hcd.models._

object Algorithm {

  // TODO remove? Only used in tests.
  protected[algorithm] def workshopsFromWorkshopChoiceId(workshops: Workshops)(choiceId: WorkshopChoiceId): Workshops =
    workshops.collect { case (wsId, ws@Workshop(_, `choiceId`, _, _)) => (wsId, ws) }

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
  private def haveDistinctField(selectedWorkshops: SelectedWorkshops, extractor: SelectedWorkshop => Any): Boolean =
    selectedWorkshops.nonEmpty &&
      selectedWorkshops
        .values
        .groupBy(extractor)
        .values
        .forall(_.size == 1)

  // empty selected workshops => false
  protected[algorithm] def haveDistinctChoiceIds(selectedWorkshops: SelectedWorkshops): Boolean =
    haveDistinctField(selectedWorkshops, _.choiceId)

  // empty selected workshops => false
  protected[algorithm] def haveDistinctTimeslots(selectedWorkshops: SelectedWorkshops): Boolean =
    haveDistinctField(selectedWorkshops, _.timeSlot)

  protected[algorithm] def possibleWorkshopCombinations(n: Int)(selectedWorkshops: SelectedWorkshops): Seq[PossibleWorkshops] =
    selectedWorkshops
      .toSeq
      .combinations(n)
      .map(_.toMap)
      .filter(haveDistinctChoiceIds)
      .filter(haveDistinctTimeslots)
      .map(_.view.mapValues { case SelectedWorkshop(category, _, _, selectionPriority) =>
        PossibleWorkshop(category, selectionPriority)
      }.toMap)
      .toSeq

}
