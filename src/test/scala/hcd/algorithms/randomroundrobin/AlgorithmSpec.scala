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

      "assigns 3 workshops to a student which has not selected any topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]))
        // student1 has no selection, thus gets assigned next best workshops, one per timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 3 topics and 1 student selecting 1 topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(1))))
        // student1 has only 1 selection, thus gets assigned 2 other workshops, one per timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 3 topics and 2 students selecting the same single topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(2))),
          student2 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(1))),
        )
        // student1 and student2 both have only 1 selection, thus get assigned 2 other workshops, one per timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1, student2), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1, student2), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1, student2), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 3 topics and 2 students selecting each a different single topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(TopicId(1) -> SelectionPriority(2))),
          student2 -> (f.grade, BiMap(TopicId(0) -> SelectionPriority(1))),
        )
        // student1 and student2 both have only 1 selection, thus get assigned 2 other workshops, one per timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student2), WorkshopId(1) -> Set(student1), WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set(student1), WorkshopId(4) -> Set(student2), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1, student2), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "assigns an alternative workshops if a student selects a non-existing topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap(TopicId(3) -> SelectionPriority(1)))) // TopicId(3) does not exist
        // student1 has no valid selection, thus gets assigned next best workshops, one per timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution for workshops for 3 topics and 1 student selecting 2 topics" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(1) -> SelectionPriority(12), // expected in 2nd timeslot, as assigned second due to lower selection prio
            TopicId(0) -> SelectionPriority(11), // expected in 1st timeslot, as assigned first due to higher selection prio
          )),
        )
        // student1 has 2 selections, thus gets assigned a 3rd workshops on the remaining timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution if workshops are not available for all timeslots" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val workshopsWsRemoved = f.workshops.removed(WorkshopId(0)).removed(WorkshopId(3))
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(1) -> SelectionPriority(11), // expected in 2nd timeslot, as assigned first due to prio, but workshop 3 does not exist, thus first timeslot cannot be used, but workshop 4 exists and second timeslot is free
            TopicId(0) -> SelectionPriority(12), // expected in 3rd timeslot, as assigned second due to prio, but workshops 0 do not exist, thus first timeslot cannot be used, workshop 1 exists but second timeslot is not free, finally workshop 2 exists and third timeslot is free
          )),
        )
        // student1 has 2 selections, thus gets assigned a 3rd workshops on the remaining timeslot
        val expectedWorkshopAssignments = Map(
          WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set(student1), // TopicId(0), WorkshopId(0) does not exist
          WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1), WorkshopId(3) does not exist
          WorkshopId(6) -> Set(student1), WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set.empty, // TopicId(2)
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "assigns a workshop if a student selects a topic which exists" +
        " but not at a timeslot not yet assigned to the student" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 4)
        val workshopsWsRemoved = f.workshops.removed(WorkshopId(0)).removed(WorkshopId(3)).removed(WorkshopId(5))
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(0) -> SelectionPriority(1), // expected in 2nd timeslot, as assigned first due to prio, but 1st timeslot does not exist.
            TopicId(1) -> SelectionPriority(2), // unassigned, as tried to assign second due to prio, but both 1st and 3rd timeslots do not exist and the 2nd is already assigned.
          )),
        )
        // student1 cannot be assigned to TopicId(1), thus needs TopicId(2) and TopicId(3) to fill the remaining timeslots
        val expectedWorkshopAssignments = Map(
          WorkshopId(1) -> Set(student1), WorkshopId(2) -> Set.empty, // TopicId(0), WorkshopId(0) does not exist
          WorkshopId(4) -> Set.empty, // TopicId(1), WorkshopId(3) and WorkshopId(5) do not exist
          WorkshopId(6) -> Set(student1), WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set.empty, // TopicId(2), student1 assigned here
          WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set(student1), // TopicId(3), student1 assigned here
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "fails the distribution if there are not enough workshops to cover all needed timeslots" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 2)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]))
        // student1 should get 3 workshops from 3 different topics, but only 2 topics exist, thus distribution fails

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics) shouldBe empty
      }

      "fails the distribution if a student selects a topic which exists but not at a timeslot not yet assigned to the student and there are not enough fall-back workshops" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val workshopsWsRemoved = f.workshops.removed(WorkshopId(0)).removed(WorkshopId(3)).removed(WorkshopId(5))
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(0) -> SelectionPriority(1), // expected in 2nd timeslot, as assigned first due to prio, but 1st timeslot does not exist.
            TopicId(1) -> SelectionPriority(2), // unassigned, as tried to assign second due to prio, but both 1st and 3rd timeslots do not exist and the 2nd is already assigned.
          )),
        )
        // student1 can be assigned to TopicId(0), but cannot be assigned to TopicId(1), thus needs 2 more topics, but only 1 more topic exists, thus distribution fails.

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics) shouldBe empty
      }

      "finds a distribution for workshops for 3 topics and 1 student selecting 3 topics" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(0) -> SelectionPriority(11), // expected in 1st timeslot, as assigned first due to prio
            TopicId(1) -> SelectionPriority(12), // expected in 2nd timeslot, as assigned second due to prio
            TopicId(2) -> SelectionPriority(13), // expected in 3rd timeslot, as assigned third due to prio
          )),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // TopicId(2)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

    }

  }

}
