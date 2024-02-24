package hcd.model

/** Metric of a combo or distribution. */
final case class Metric(m: Int) extends AnyVal

object Metric {

  private val neutralMetric: Metric = Metric(0)

  val initialMetric: Metric = neutralMetric

  def add(m: Metric, ms: Metric*): Metric = ms.fold(m) { case (m1, m2) => Metric(m1.m + m2.m) }

  def metric(topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics)(workshopAssignments: WorkshopAssignments): Metric = {
    val studentAssignments = studentAssignmentsFrom(workshopAssignments)
    val studentMetrics = studentAssignments.map { case (studentId, workshopIds) =>
      val assignedTopicsIds = workshopIds.map(workshops).toList.map { case (topicId, _, _, _) => topicId } // .toList is redundant to business logic
      val (_, selectedTopics) = studentsSelectedTopics(studentId)
      val selectionPriorities = assignedTopicsIds.map(selectedTopics.get).map(_.getOrElse(SelectionPriority(7)))
      val categories = assignedTopicsIds.map(topics)
      val metricPrios = metricFromSelectionPriorities(selectionPriorities)
      val metricCategories = metricFromCategories(categories)
      studentId -> add(metricPrios, metricCategories)
    }
    studentMetrics.values.toList match {
      case ::(head, next) => add(head, next: _*)
      case Nil => neutralMetric
    }
  }

  /** Simple linear metric on priorities. */
  def metricFromSelectionPriorities(prios: Iterable[SelectionPriority]): Metric = Metric(prios.map(_.prio).sum)

  /** Malus if a combo contains only sports category. */
  def metricFromCategories(categories: Iterable[Category]): Metric =
    if (categories.forall(_ == Sports)) Metric(1000)
    else neutralMetric

}
