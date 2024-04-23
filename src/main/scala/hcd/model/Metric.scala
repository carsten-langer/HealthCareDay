package hcd.model

import com.typesafe.scalalogging.StrictLogging

/** Metric of a combo or distribution. */
final case class Metric(m: Int) extends AnyVal

object Metric extends StrictLogging {

  private val neutralMetric = Metric(0)
  private val bonusMetricGroup = Metric(-6) // compensation for simple metrics for selection prios (1 + 2 + 3), see below for details
  private val malusMetricNoneOfFirstPrios = Metric(10000)
  private val malusMetricUnwantedTopic = Metric(7)
  private val malusMetricSports = Metric(1000)
  private val malusMetricSparseWorkshop = Metric(10000)

  val initialMetric: Metric = neutralMetric

  def add(m1: Metric, m2: Metric): Metric = Metric(m1.m + m2.m)

  def add(m: Metric, ms: Iterable[Metric]): Metric = ms.fold(m)(add)

  def metricGlobal(topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics)(workshopAssignments: WorkshopAssignments): Metric = {
    val metricStudents = orderedMetricsStudents(topics, workshops, studentsSelectedTopics)(workshopAssignments) match {
      case Nil => initialMetric
      case ::(head, next) => add(head, next)
    }
    add(metricStudents, metricWorkshops(workshopAssignments))
  }

  def orderedMetricsStudents(topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics)(workshopAssignments: WorkshopAssignments): List[Metric] =
    studentAssignmentsFrom(workshopAssignments)
      .toList
      .sortBy { case (StudentId(id), _) => id }
      .map { case (studentId, assignedWorkshopIds) =>
        val (_, selectedTopics) = studentsSelectedTopics(studentId)
        metricStudent(topics, workshops)(assignedWorkshopIds, selectedTopics)
      }

  def metricStudent(topics: Topics, workshops: Workshops)(assignedWorkshopIds: Set[WorkshopId], selectedTopics: SelectedTopics): Metric = {
    val assignedTopicIds = assignedWorkshopIds.map(workshops).toList.map { case (topicId, _, _, _) => topicId } // .toList is redundant to business logic
    val assignedCategories = assignedTopicIds.map(topics)
    val metricCategories = metricFromCategories(assignedCategories)
    val metricSelectedTopics =
      if (selectedTopics.isEmpty)
        // The student did not chose any topics. There is no malus, as the student cannot complain about any assigned
        // topic, as no selection was done.
        neutralMetric
      else {
        // The student did chose topics and we expect the student to have selected enough topics that all 3 timeslots
        // could be filled. However, we do not hard assert it, as some unit tests may draw profit from setting up such
        // normally unexpected situation. However, we log an error in this case.
        if (selectedTopics.size < allTimeSlots.size) logger.error(
          s"If a student made selections, at least ${allTimeSlots.size} selections should have been made, but only ${selectedTopics.size} were made!")
        // In this case, a student being assigned the topics of the first 3 selection priorities would without
        // compensation get a metric of 1 + 2 + 3 = 6, and thus a worse metric than a student having made no selection
        // and getting the metric 0.
        // To compensate for this, for the set of selection priorities we calculate the normal the metric from each
        // selection priority, but add a bonus of (-6) to the group, which compensates this effect.
        // If the student got assigned workshops without selecting the topic, while having selected topics she was
        // not assigned to, this gives a malus per workshop assigned and not be part of the selection.
        val assignedSelectedTopics = selectedTopics.filter { case (topicId, _) => assignedTopicIds.contains(topicId) }
        val malusNoneOfFirstThreePrios =
          if (selectedTopics.isEmpty
            || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
            prio <= 3 && assignedTopicIds.contains(topicId)
          }) neutralMetric // no malus if no topic was selected or from the selected at least one topic with prio <= 3 was assigned
          else malusMetricNoneOfFirstPrios
        val malusesUnwantedTopics = List.fill(allTimeSlots.size - assignedSelectedTopics.size)(malusMetricUnwantedTopic)
        val malus = add(malusNoneOfFirstThreePrios, malusesUnwantedTopics)
        val bonusMalus = add(bonusMetricGroup, malus)
        val assignedSelectionPriorities = assignedSelectedTopics.values
        val metricsSelectionPriorities = assignedSelectionPriorities.map(metricFromSelectionPriority)
        add(bonusMalus, metricsSelectionPriorities)
      }
    add(metricCategories, metricSelectedTopics)
  }

  /** Simple linear metric from selection priority without group compensation. */
  def metricFromSelectionPriority(selectionPriority: SelectionPriority): Metric = Metric(selectionPriority.prio)

  /** Malus if a combo contains only sports category. */
  def metricFromCategories(categories: Iterable[Category]): Metric =
    if (categories.forall(_ == Sports)) malusMetricSports
    else neutralMetric

  def metricWorkshops(workshopAssignments: WorkshopAssignments): Metric =
    add(neutralMetric, workshopAssignments.map { case (_, students) => metricWorkshop(students.size) })

  def metricWorkshop(filledSeats: Int): Metric =
    if (filledSeats >= 1 && filledSeats <= 5) malusMetricSparseWorkshop
    else neutralMetric

}
