package hcd.algorithms

import hcd.model._

trait FixtureWorkshops {
  def topics: Topics

  def workshops: Workshops

  def noSeats: Int

  def zeroSeats: Seats = Seats(0)

  def sex: Sex = Divers

  def grade: Grade = Grade(0) // a test grade for all students, included in the workshops, value 0 has no further meaning

  def gradeNonMatching: Grade = Grade(1) // a test grade not matching normal grade

  def sexes: Set[Sex] = Set(Divers, Female, Male) // set of sexes for all workshops

  def grades: Set[Grade] = Set(grade) // set of grades for all workshops
}
