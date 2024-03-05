package hcd.model

/** Metric of a combo or distribution. */
final case class Metric(m: Int) extends AnyVal

object Metric {

  private val neutralMetric: Metric = Metric(0)

  val initialMetric: Metric = neutralMetric

  def add(m1: Metric, m2: Metric): Metric = Metric(m1.m + m2.m)

  def add(m: Metric, ms: Iterable[Metric]): Metric = ms.fold(m)(add)

  def metric(topics: Topics, workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics)(workshopAssignments: WorkshopAssignments): Metric = {
    val studentAssignments = studentAssignmentsFrom(workshopAssignments)
    val studentMetrics = studentAssignments.map { case (studentId, workshopIds) =>
      val assignedTopicsIds = workshopIds.map(workshops).toList.map { case (topicId, _, _, _) => topicId } // .toList is redundant to business logic
      val (_, selectedTopics) = studentsSelectedTopics(studentId)
      val selectionPriorities = assignedTopicsIds.map(selectedTopics.get).map(_.getOrElse(SelectionPriority(7)))
      val categories = assignedTopicsIds.map(topics)
      val metricsPrios = selectionPriorities.map(metricFromSelectionPriority)
      val metricCategories = metricFromCategories(categories)
      studentId -> add(metricCategories, metricsPrios)
    }
    studentMetrics.values.toList match {
      case ::(head, next) => add(head, next)
      case Nil => neutralMetric
    }
  }

  /** Simple linear metric from selection priority. */
  def metricFromSelectionPriority(selectionPriority: SelectionPriority): Metric = Metric(selectionPriority.prio)

  /** Malus if a combo contains only sports category. */
  def metricFromCategories(categories: Iterable[Category]): Metric =
    if (categories.forall(_ == Sports)) Metric(1000)
    else neutralMetric

}
