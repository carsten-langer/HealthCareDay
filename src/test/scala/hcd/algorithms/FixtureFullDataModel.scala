package hcd.algorithms

import hcd.model._
import io.cvbio.collection.mutable.bimap.BiMap

import scala.util.Random

trait FixtureFullDataModel extends FixtureWorkshops {
  // Inputs for model size
  private val noTopics = 50
  private val noStudents = 600
  private val noSelectionsPerStudent = 6
  // combo 50/1000/6/30-25 finds very quick a distribution
  // combo 50/1000/6/24-20 searches a lot (20 is min.)
  // combo 50/600/6/16 finds very quick a distribution
  // combo 50/600/6/15 searches a lot (12 is min.)
  override val noSeats = 16
  private val underlyingFixtureWorkshops: FixtureWorkshops = fixtureSymmetricWorkshopsFor(noTopics, noSeats)
  override val topics: Topics = underlyingFixtureWorkshops.topics
  override val workshops: Workshops = underlyingFixtureWorkshops.workshops
  private lazy val studentIds: Set[StudentId] = Range(0, noStudents).toSet.map(StudentId)
  private lazy val selectionPriorities: Set[SelectionPriority] = Range.inclusive(1, noSelectionsPerStudent).toSet.map(SelectionPriority(_))

  // generate random workshop selections
  Random.setSeed(0L) // fix randomness during development
  lazy val studentsSelectedTopics: StudentsSelectedTopics = studentIds.map(
    _ -> (grade, BiMap.from(Random.shuffle(topics.keySet.toSeq).zip(selectionPriorities)))
  ).toMap
}
