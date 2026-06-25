package hcd.model

/** Selection priority, e.g. 1 to 6. */
final case class SelectionPriority(prio: Int) extends AnyVal

object SelectionPriority {
  val WorstPrio: SelectionPriority = SelectionPriority(6)
  val UnselectedPrio: SelectionPriority = SelectionPriority(0)
  val UnwantedSelectionPrio: SelectionPriority = SelectionPriority(7)
}
