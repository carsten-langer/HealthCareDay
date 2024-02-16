package hcd.model

/** A workshop is determined by a topic and a timeslot. This combination determines a concrete workshop. */
final case class TopicTimeslot(topicId: TopicId, timeSlot: TimeSlot)
