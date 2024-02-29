package hcd.algorithms.randomroundrobin

import hcd.algorithms.fixtureSymmetricWorkshopsFor
import hcd.algorithms.randomroundrobin.Algorithm.distributionAlgorithm
import hcd.model.{SelectionPriority, StudentId, TopicId, WorkshopId}
import io.cvbio.collection.mutable.bimap.BiMap
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AlgorithmSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues {

  "Algorithm" should {

    "provide a distributionAlgorithm" which {

      "finds the empty distribution if there are neither topics/workshops nor students selections to distribute" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 0)

        distributionAlgorithm(Map.empty, Map.empty)(Map.empty).value shouldBe empty
        distributionAlgorithm(Map.empty, f.workshops)(Map.empty).value shouldBe empty
        distributionAlgorithm(f.topics, f.workshops)(Map.empty).value shouldBe empty
      }

      "finds the empty distribution if there are workshops but no students selections to distribute" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val expectedWorkshopAssignments = f.workshops.view.mapValues(_ => Set.empty).toMap

        distributionAlgorithm(Map.empty, f.workshops)(Map.empty).value shouldEqual expectedWorkshopAssignments
        distributionAlgorithm(f.topics, f.workshops)(Map.empty).value shouldEqual expectedWorkshopAssignments
      }

      "assigns a workshop to a student which has not selected any topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]))
        // student1 has no selection, thus gets assigned next best workshop
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 1 topic and 1 student selecting 1 topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(1))))
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 1 topic and 2 students selecting the same single topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(2))),
          student2 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(1))),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1, student2), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 2 topics and 2 students selecting each a different single topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 2)
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(TopicId(1) -> SelectionPriority(2))),
          student2 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(1))),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student2), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set(student1), WorkshopId(4) -> Set.empty, WorkshopId(5) -> Set.empty, // TopicId(1)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "assigns a workshop if a student selects a non-existing topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap(TopicId(1) -> SelectionPriority(1)))) // TopicId(1) does not exist
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 2 topics and 1 student selecting 2 topics" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 2)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(1) -> SelectionPriority(12), // expected in 2nd timeslot, as assigned second due to lower selection prio
            TopicId(0) -> SelectionPriority(11), // expected in 1st timeslot, as assigned first due to higher selection prio
          )),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution if workshops are not available for all timeslots" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 2)
        val workshopsWsRemoved = f.workshops.removed(WorkshopId(0)).removed(WorkshopId(3))
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(1) -> SelectionPriority(11), // expected in 2nd timeslot, as assigned first due to prio, but workshop 3 does not exist, thus first timeslot cannot be used, but workshop 4 exists and second timeslot is free
            TopicId(0) -> SelectionPriority(12), // expected in 3rd timeslot, as assigned second due to prio, but workshops 0 do not exist, thus first timeslot cannot be used, workshop 1 exists but second timeslot is not free, finally workshop 2 exists and third timeslot is free
          )),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set(student1), // TopicId(0), WorkshopId(0) does not exist
          WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1), WorkshopId(3) does not exist
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      // todo fixme: The current algo does not assign a replacement workshop in this situation.
      // This should be fixed and this test case changed.
      "does not (yet) assign a workshop if a student selects a topic which exists" +
        " but not at a timeslot not yet assigned to the student" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val workshopsWsRemoved = f.workshops.removed(WorkshopId(0)).removed(WorkshopId(3)).removed(WorkshopId(5))
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(0) -> SelectionPriority(1), // expected in 2nd timeslot, as assigned first due to prio, but 1st timeslot does not exist.
            TopicId(1) -> SelectionPriority(2), // unassigned, as tried to assign second due to prio, but both 1st and 3rd timeslots do not exist and the 2nd is already assigned.
          )),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(1) -> Set(student1), WorkshopId(2) -> Set.empty, // TopicId(0), WorkshopId(0) does not exist
          WorkshopId(4) -> Set.empty, // TopicId(1), WorkshopId(3) and WorkshopId(5) do not exist
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set.empty, // TopicId(2), student1 not (yet) assigned here
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

    }

  }

}
