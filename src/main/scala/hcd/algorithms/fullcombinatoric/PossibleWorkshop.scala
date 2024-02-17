package hcd.algorithms.fullcombinatoric

import hcd.model.{Category, SelectionPriority}

// Only the attributes of a possible workshop which are relevant to the algorithm to find the perfect distribution.
private final case class PossibleWorkshop(category: Category, selectionPriority: SelectionPriority)
