package hcd.model

import com.typesafe.scalalogging.LazyLogging
import hcd.model

/** Metric of a combo or distribution. */
final case class Metric(m: Int) extends AnyVal

object Metric extends LazyLogging {

  private val neutralMetric = Metric(0)
  private val bonusMetricGroup = Metric(-6) // compensation for simple metrics for selection prios (1 + 2 + 3), see below for details
  private val malusMetricNoneOfFirstPrios = Metric(10_000)
  private val malusMetricUnwantedTopic = Metric(7)
  private val malusMetricAllSports = Metric(1_000)
  private val malusMinSeatsNotReached = Metric(1_000_000)
  private val malusMetricSparseWorkshop = Metric(100_000)

  private val initialMetric: Metric = neutralMetric

  private def add(m1: Metric, m2: Metric): Metric = Metric(m1.m + m2.m)

  private def add(m: Metric, ms: Iterable[Metric]): Metric = ms.fold(m)(add)

  def metricGlobal(topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics)(workshopAssignments: WorkshopAssignments): Metric = {
    val metricStudents =
      orderedMetricsStudents(topics, workshops, studentsSelectedTopics)(workshopAssignments)
        .fold(initialMetric)(add)
    add(metricStudents, metricWorkshops(workshops)(workshopAssignments))
  }

  def orderedMetricsStudents(topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics)(workshopAssignments: WorkshopAssignments): List[Metric] =
    studentAssignmentsFrom(workshopAssignments)
      .toList
      .sortBy { case (StudentId(id), _) => id }
      .map { case (studentId, assignedWorkshopIds) =>
        val (_, _, selectedTopics) = studentsSelectedTopics(studentId)
        metricStudent(topics, workshops)(studentId, assignedWorkshopIds, selectedTopics)
      }

  def metricStudent(topics: Topics, workshops: Workshops)(studentId: StudentId, assignedWorkshopIds: Set[WorkshopId], selectedTopics: SelectedTopics): Metric = {
    val assignedTopicIds = assignedWorkshopIds.map(workshops).toList.map { case (topicId, _, _, _, _, _) => topicId } // .toList is redundant to business logic
    val assignedCategories = assignedTopicIds.map(topics).map { case (_, category, _, _) => category }
    val metricCategories = metricFromCategories(assignedCategories)
    val metricSelectedTopics =
      if (selectedTopics.isEmpty)
        // The student did not choose any topics. There is no malus, as the student cannot complain about any assigned
        // topic, as no selection was done.
        neutralMetric
      else {
        // The student did choose topics, and we expect the student to have selected enough topics that all 3 timeslots
        // could be filled. However, we do not hard assert it, as some unit tests may profit from setting up such a
        // normally unexpected situation. However, we log an error in this case.
        // TODO Quirk for HDC26 for student 332, this student only had 1 preassigned selection.
        if (selectedTopics.size < model.allTimeSlots.size && studentId.id != 332) logger.error(
          s"If a student made selections, at least ${model.allTimeSlots.size} selections should have been made, but for studentId $studentId only ${selectedTopics.size} were made!"
        )
        // In this case, a student being assigned the topics of the first 3 selection priorities would without
        // compensation get a metric of 1 + 2 + 3 = 6, and thus a worse metric than a student having made no selection
        // and getting the metric 0.
        // To compensate for this, for the set of selection priorities we calculate the normal metric from each
        // selection priority but add a bonus of (-6) to the group, which compensates for this effect.
        // If the student got assigned workshops without selecting the topic, while having selected topics she was
        // not assigned to, this gives a malus per workshop assigned and not be part of the selection.
        val assignedSelectedTopics = selectedTopics.filter { case (topicId, _) => assignedTopicIds.contains(topicId) }
        val malusNoneOfFirstThreePrios =
          if (selectedTopics.isEmpty
            || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
            prio <= 3 && assignedTopicIds.contains(topicId)
          }) neutralMetric // no malus if no topic was selected or from the selected at least one topic with prio <= 3 was assigned
          else malusMetricNoneOfFirstPrios
        val malusesUnwantedTopics = List.fill(model.allTimeSlots.size - assignedSelectedTopics.size)(malusMetricUnwantedTopic)
        val malus = add(malusNoneOfFirstThreePrios, malusesUnwantedTopics)
        val bonusMalus = add(bonusMetricGroup, malus)
        val assignedSelectionPriorities = assignedSelectedTopics.values
        val metricsSelectionPriorities = assignedSelectionPriorities.map(metricFromSelectionPriority)
        add(bonusMalus, metricsSelectionPriorities)
      }
    add(metricCategories, metricSelectedTopics)
  }

  /**
   * Simple linear metric from selection priority without group compensation.
   * Leading to total metrics, thereby accounting for group compensation of 6:
   * 00 + 6 = 1 + 2 + 3
   * 01 + 6 = 1 + 2 + 4
   * 02 + 6 = 1 + 2 + 5 = 1 + 3 + 4
   * 03 + 6 = 1 + 2 + 6 = 1 + 3 + 5 = 2 + 3 + 4
   * 04 + 6 = 1 + 2 + 7 = 1 + 3 + 6 = 1 + 4 + 5 = 2 + 3 + 5
   * 05 + 6 = 1 + 3 + 7 = 1 + 4 + 6 = 2 + 3 + 6 = 2 + 4 + 5
   * 06 + 6 = 1 + 4 + 7 = 1 + 5 + 6 = 2 + 3 + 7 = 2 + 4 + 6
   * 07 + 6 = 1 + 5 + 7 = 2 + 4 + 7 = 2 + 5 + 6 = 3 + 4 + 6
   * 08 + 6 = 1 + 6 + 7 = 2 + 5 + 7 = 3 + 4 + 7 = 3 + 5 + 6
   * 09 + 6 = 1 + 7 + 7 = 2 + 6 + 7 = 3 + 5 + 7
   * 10 + 6 = 2 + 7 + 7 = 3 + 6 + 7 = 4 + 5 + 7
   * 11 + 6 = 3 + 7 + 7 = 4 + 6 + 7
   * 12 + 6 = 4 + 7 + 7 = 5 + 6 + 7
   * 13 + 6 = 5 + 7 + 7
   * 14 + 6 = 6 + 7 + 7
   * 15 + 6 = 7 + 7 + 7
   */
  private def metricFromSelectionPriority(selectionPriority: SelectionPriority): Metric = Metric(selectionPriority.prio)

  /** Malus if a combo contains only sports category. */
  private def metricFromCategories(categories: Iterable[Category]): Metric =
    if (categories.forall(_ == Sports)) malusMetricAllSports
    else neutralMetric

  def metricWorkshops(workshops: Workshops)(workshopAssignments: WorkshopAssignments): Metric =
    add(neutralMetric, workshopAssignments.map {
      case (workshopId, students) => metricWorkshop(workshops)(workshopId, students.size)
    })

  def metricWorkshop(workshops: Workshops)(workshopId: WorkshopId, filledSeats: Int): Metric = {
    val (_, _, _, _, Seats(minSeats), _) = workshops(workshopId)
    if (filledSeats < minSeats) malusMinSeatsNotReached
    else if (filledSeats > 0 && filledSeats < 6) malusMetricSparseWorkshop
    else neutralMetric
  }

}
