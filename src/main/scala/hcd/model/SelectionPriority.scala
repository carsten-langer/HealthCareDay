package hcd.model

/** Selection priority, e.g. 1 to 6. */
final case class SelectionPriority(prio: Int) extends AnyVal

object SelectionPriority {
  val worstPrio: SelectionPriority = SelectionPriority(6)
}
