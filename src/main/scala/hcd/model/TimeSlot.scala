package hcd.model

import enumeratum._

sealed abstract class TimeSlot(val ts: Int) extends EnumEntry

case object TimeSlot extends Enum[TimeSlot] {

  case object FirstTimeSlot extends TimeSlot(1)

  case object SecondTimeSlot extends TimeSlot(2)

  case object ThirdTimeSlot extends TimeSlot(3)

  val values: IndexedSeq[TimeSlot] = findValues

}
