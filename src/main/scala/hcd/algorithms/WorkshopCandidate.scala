package hcd.algorithms

import hcd.model.{Category, SelectionPriority, TimeSlot, TopicId}

// Collects temporarily all attributes of a workshop candidate which, if possible, could then be part of a  workshop combo.
private final case class WorkshopCandidate(topicId: TopicId, timeSlot: TimeSlot, category: Category, selectionPriority: SelectionPriority)
