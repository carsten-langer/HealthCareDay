package hcd.model

/** Metric of a combo or distribution. */
final case class Metric(m: Int) extends AnyVal

object Metric {

  private val neutralMetric: Metric = Metric(0)

  val initialMetric: Metric = neutralMetric

  def add(m: Metric, ms: Metric*): Metric = ms.fold(m) { case (m1, m2) => Metric(m1.m + m2.m) }

  def withMetric[A](distributionAlgorithm: DistributionAlgorithm[A]): DistributionAlgorithm[(Metric, A)] =
    (topics: Topics, workshops: Workshops) => (studentsSelectedTopics: StudentsSelectedTopics) =>
      distributionAlgorithm(topics, workshops)(studentsSelectedTopics)
        .map { case (workshopAssignments, a) =>
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
          val metric = studentMetrics.values.toList match {
            case head :: tail => add(head, tail: _*)
            case Nil => neutralMetric
          }
          workshopAssignments -> (metric, a)
        }

  /** Simple linear metric on priorities. */
  def metricFromSelectionPriorities(prios: Iterable[SelectionPriority]): Metric = Metric(prios.map(_.prio).sum)

  /** Malus if a combo contains only sports category. */
  def metricFromCategories(categories: Iterable[Category]): Metric =
    if (categories.forall(_ == Sports)) Metric(1000)
    else neutralMetric

}
