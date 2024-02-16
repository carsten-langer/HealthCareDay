package hcd.model

sealed trait TimeSlot

case object FirstTimeSlot extends TimeSlot

case object SecondTimeSlot extends TimeSlot

case object ThirdTimeSlot extends TimeSlot
