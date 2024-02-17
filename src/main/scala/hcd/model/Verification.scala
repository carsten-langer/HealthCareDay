package hcd.model

import com.typesafe.scalalogging.StrictLogging

object Verification extends StrictLogging {

  def withVerification[A](distributionAlgorithm: DistributionAlgorithm[A]): DistributionAlgorithm[A] =
    (topics: Topics, workshops: Workshops, workshopSeats: WorkshopSeats) =>
      (studentsSelectedTopics: StudentsSelectedTopics) =>
        if (isValidInput(topics, workshops, workshopSeats, studentsSelectedTopics))
          distributionAlgorithm(topics, workshops, workshopSeats)(studentsSelectedTopics) match {
            case result@Some((workshopAssignments, _)) if isValidResult(workshops, studentsSelectedTopics, workshopAssignments) => result
            case _ => None
          }
        else None

  private def isValidInput(topics: Topics, workshops: Workshops, workshopSeats: WorkshopSeats, studentsSelectedTopics: StudentsSelectedTopics): Boolean =
    workshopsHaveKnownTopic(workshops, topics) &&
      workshopsHaveKnownSeats(workshops, workshopSeats) &&
      seatsArePositive(workshopSeats) &&
      studentsSelectedTopicsHaveKnownTopic(studentsSelectedTopics, topics) &&
      studentsSelectedTopicsHaveSelectionPrioritiesInRange(studentsSelectedTopics)

  private def isValidResult(workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics, workshopAssignments: WorkshopAssignments): Boolean =
    workshopsContainedInWorkshopAssignments(workshops, workshopAssignments) &&
      studentsHaveAssignments(workshops, studentsSelectedTopics, workshopAssignments)

  private def workshopsHaveKnownTopic(workshops: Workshops, topics: Topics): Boolean = {
    val b = workshops.values.map(_.topicId).forall(topics.contains)
    if (!b) logger.error("A workshop contains an unknown topic.")
    b
  }

  private def workshopsHaveKnownSeats(workshops: Workshops, workshopSeats: WorkshopSeats): Boolean = {
    val b = workshops.keys.forall(workshopSeats.contains)
    if (!b) logger.error("A workshop does not have a seats information.")
    b
  }

  private def seatsArePositive(workshopSeats: WorkshopSeats): Boolean = {
    val b = workshopSeats.forall { case (_, Seats(n)) => n > 0 }
    if (!b) logger.error("A non-positive seats exist.")
    b
  }

  private def studentsSelectedTopicsHaveKnownTopic(studentsSelectedTopics: StudentsSelectedTopics, topics: Topics): Boolean = {
    val b = studentsSelectedTopics.values.flatMap(_.keys).forall(topics.contains)
    if (!b) logger.error("A studentsSelectedTopics contains an unknown topic.")
    b
  }

  private def studentsSelectedTopicsHaveSelectionPrioritiesInRange(studentsSelectedTopics: StudentsSelectedTopics): Boolean = {
    val validRange = Range.inclusive(1, 6)
    val b = studentsSelectedTopics.values.flatMap(_.values).map(_.prio).forall(validRange.contains)
    if (!b) logger.error("A studentsSelectedTopics contains a selection priority out of range.")
    b
  }

  private def workshopsContainedInWorkshopAssignments(workshops: Workshops, workshopAssignments: WorkshopAssignments): Boolean = {
    val b = workshops.keys.forall(workshopAssignments.contains)
    if (!b) logger.error("A workshop is not contained in workshop assignments.")
    b
  }

  private def studentsHaveAssignments(workshops: Workshops, studentsSelectedTopics: StudentsSelectedTopics, workshopAssignments: WorkshopAssignments): Boolean = {
    val b1 = workshopAssignments.keys.forall(workshops.contains)
    if (!b1) logger.error("A workshop assignment contains an unknown workshop.")
    val studentNumberOfAssignments = studentAssignmentsFrom(workshopAssignments).view.mapValues(_.size)
    val b2 = studentsSelectedTopics.keys.forall(studentNumberOfAssignments.getOrElse(_, 0) == 3)
    if (!b2) logger.error("A student is not assigned to 3 workshops.")
    b1 //&& b2  // todo re-enable the result of check b2
  }

}
