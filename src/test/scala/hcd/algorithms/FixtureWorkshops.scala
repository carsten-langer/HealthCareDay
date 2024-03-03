package hcd.algorithms

import hcd.model.{Grade, Topics, Workshops}

trait FixtureWorkshops {
  def topics: Topics

  def workshops: Workshops

  def noSeats: Int

  def grade: Grade = Grade(0) // a test grade for all students, included in the workshops, the value 0 has no further meaning

  def gradeNonMatching: Grade = Grade(1) // a test grade not matching normal grade

  def grades: Set[Grade] = Set(grade) // set of grades for all workshops
}
