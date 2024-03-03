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

      "finds a distribution which respects that not all 3 workshops shall be of category nutrition" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 10)
        // remove workshops for topics 1, 2, 3, 5, 6, 7, so that only topics 0, 4, 8, 9 are left
        val workshopsWsRemoved = f.workshops.filter { case (_, (TopicId(id), _, _, _)) => Set(0, 4, 8, 9).contains(id) }
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(0) -> SelectionPriority(1), // nutrition
            TopicId(4) -> SelectionPriority(2), // nutrition
            TopicId(8) -> SelectionPriority(3), // nutrition
          )),
          student2 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]),
        )
        // student 1 can be assigned topic 0 and topic 4, but not topic 8, as this would be the 3rd nutrition.
        // As topic 1, 2, 3, 5, 6, 7, do not exist, topic 9 is the replacement, i.e. the assignment is 0, 4, 9.
        // student 2 has no selection, thus would get assigned topics 0, 1, 2 if they existed.
        // As some do not exist, she would get assigned 0, 4, 8, but this is 3 times nutrition,
        // thus the assignment is 0, 4, 9.
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1, student2), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
          // workshops for topics 1, 2, 3 do not exist
          WorkshopId(12) -> Set.empty, WorkshopId(13) -> Set(student1, student2), WorkshopId(14) -> Set.empty, // TopicId(4)
          // workshops for topics 5, 6, 7 do not exist
          WorkshopId(24) -> Set.empty, WorkshopId(25) -> Set.empty, WorkshopId(26) -> Set.empty, // TopicId(8)
          WorkshopId(27) -> Set.empty, WorkshopId(28) -> Set.empty, WorkshopId(29) -> Set(student1, student2), // TopicId(9)
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "fails the distribution if the rule no-3-nutrition cannot be fulfilled" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 9)
        // remove workshops for topics 1, 2, 3, 5, 6, 7, so that only topics 0, 4, 8 are left
        val workshopsWsRemoved = f.workshops.filter { case (_, (TopicId(id), _, _, _)) => Set(0, 4, 8).contains(id) }
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]))
        // student 1 has no selection, thus would get assigned topics 0, 1, 2 if they existed.
        // As some do not exist, she would get assigned 0, 4, 8, but this is 3 times nutrition.
        // As no topic 9 exists, the distribution fails.

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics) shouldBe empty
      }

      "finds a distribution which respects that not all 3 workshops shall be of category relaxation" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 11)
        // remove workshops for topics 0, 2, 3, 4, 6, 7, 8, so that only topics 1, 5, 9, 10 are left
        val workshopsWsRemoved = f.workshops.filter { case (_, (TopicId(id), _, _, _)) => Set(1, 5, 9, 10).contains(id) }
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(1) -> SelectionPriority(1), // relaxation
            TopicId(5) -> SelectionPriority(2), // relaxation
            TopicId(9) -> SelectionPriority(3), // relaxation
          )),
          student2 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]),
        )
        // student 1 can be assigned topic 1 and topic 5, but not topic 9, as this would be the 3rd relaxation.
        // As topic 0, 2, 3, 4, 6, 7, 8 do not exist, topic 10 is the replacement, i.e. the assignment is 1, 5, 10.
        // student 2 has no selection, thus would get assigned topics 0, 1, 2 if they existed.
        // As some do not exist, she would get assigned 1, 5, 9, but this is 3 times relaxation,
        // thus the assignment is 1, 5, 10.
        val expectedWorkshopAssignments = Map(
          // workshops for topic 0 does not exist
          WorkshopId(3) -> Set(student1, student2), WorkshopId(4) -> Set.empty, WorkshopId(5) -> Set.empty, // TopicId(1)
          // workshops for topics 2, 3, 4 do not exist
          WorkshopId(15) -> Set.empty, WorkshopId(16) -> Set(student1, student2), WorkshopId(17) -> Set.empty, // TopicId(5)
          // workshops for topics 6, 7, 8 do not exist
          WorkshopId(27) -> Set.empty, WorkshopId(28) -> Set.empty, WorkshopId(29) -> Set.empty, // TopicId(9)
          WorkshopId(30) -> Set.empty, WorkshopId(31) -> Set.empty, WorkshopId(32) -> Set(student1, student2), // TopicId(10)
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "fails the distribution if the rule no-3-relaxation cannot be fulfilled" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 10)
        // remove workshops for topics 0, 2, 3, 4, 6, 7, 8, so that only topics 1, 5, 9 are left
        val workshopsWsRemoved = f.workshops.filter { case (_, (TopicId(id), _, _, _)) => Set(1, 5, 9).contains(id) }
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]))
        // student 1 has no selection, thus would get assigned topics 0, 1, 2 if they existed.
        // As some do not exist, she would get assigned 1, 5, 9, but this is 3 times relaxation.
        // As no topic 10 exists, the distribution fails.

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics) shouldBe empty
      }

      "finds a distribution which avoids that all 3 workshops shall are of category sports" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 12)
        // remove workshops for topics 0, 1, 3, 4, 5, 7, 8, 9, so that only topics 2, 6, 10, 11 are left
        val workshopsWsRemoved = f.workshops.filter { case (_, (TopicId(id), _, _, _)) => Set(2, 6, 10, 11).contains(id) }
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(2) -> SelectionPriority(1), // sports
            TopicId(6) -> SelectionPriority(2), // sports
            TopicId(10) -> SelectionPriority(3), // sports
          )),
          student2 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]),
        )
        // student 1 can be assigned topic 2 and topic 6. She could also be assigned topic 10, if really needed, but
        // this is avoided if possible. As topics 0, 1, 3, 4, 5, 7, 8, 9 do not exist, but topic 11 exists,
        // it serves as a replacement, i.e. the assignment is 2, 6, 11.
        // student 2 has no selection, thus would get assigned topics 0, 1, 2 if they existed. As some do not exist,
        // she would get assigned 2, 6, 10, but this is 3 times sports, which should be avoided if possible.
        // As topic 11 exists, it serves as a replacement, i.e. the assignment is 2, 6, 11.
        val expectedWorkshopAssignments = Map(
          // workshops for topics 0, 1 do not exist
          WorkshopId(6) -> Set(student1, student2), WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set.empty, // TopicId(2)
          // workshops for topics 3, 4, 5 do not exist
          WorkshopId(18) -> Set.empty, WorkshopId(19) -> Set(student1, student2), WorkshopId(20) -> Set.empty, // TopicId(6)
          // workshops for topics 7, 8, 9 do not exist
          WorkshopId(30) -> Set.empty, WorkshopId(31) -> Set.empty, WorkshopId(32) -> Set.empty, // TopicId(10)
          WorkshopId(33) -> Set.empty, WorkshopId(34) -> Set.empty, WorkshopId(35) -> Set(student1, student2), // TopicId(11)
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution which assigns all 3 workshops having category sports if no alternative exists" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 15)
        // remove workshops for topics 0, 1, 3, 4, 5, 7, 8, 9, 11, 12, 13 so that only topics 2, 6, 10, 14 are left
        val workshopsWsRemoved = f.workshops.filter { case (_, (TopicId(id), _, _, _)) => Set(2, 6, 10, 14).contains(id) }
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(2) -> SelectionPriority(1), // sports
            TopicId(6) -> SelectionPriority(2), // sports
            TopicId(14) -> SelectionPriority(3), // sports
          )),
          student2 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]),
        )
        // student 1 can be assigned topic 2 and topic 6. She could also be assigned topic 10 or 14, if really needed, but
        // this is avoided if possible. As topics 0, 1, 3, 4, 5, 7, 8, 9, 11, 12, 13 do not exist, and also no topic 15
        // exists, there is no non-sports replacement, thus actually either topic 10 or 14 must be assigned.
        // todo optimize distribution algorithm in case finally 3 times sports shall be assigned and student selected 3 times sport
        // The ideal assignment would be topic 14, as this is what the student selected. However, the current algorithm,
        // at the point in time when finally 3-sports shall be assigned as last rescue, has already forgotten about the
        // student's selection and will therefore select the next possible workshop, here from topic 10, even though this
        // topic 10 was not originally selected by the student, i.e. the assignment is 2, 6, 10.
        // This test case documents the current outcome and must be changed if the distribution algorithm is optimized for
        // this case.
        // student 2 has no selection, thus would get assigned topics 0, 1, 2 if they existed. As some do not exist,
        // she would get assigned 2, 6, and 10 or 14, but this is 3 times sports, which should be avoided if possible.
        // As no replacement topic exists, actually topic 10 must be assigned, i.e. the assignment is 2, 6, 10.
        // The topic selection for student 2 would not change if the topic selection for student 1 was optimized.
        val expectedWorkshopAssignments = Map(
          // workshops for topics 0, 1 do not exist
          WorkshopId(6) -> Set(student1, student2), WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set.empty, // TopicId(2)
          // workshops for topics 3, 4, 5 do not exist
          WorkshopId(18) -> Set.empty, WorkshopId(19) -> Set(student1, student2), WorkshopId(20) -> Set.empty, // TopicId(6)
          // workshops for topics 7, 8, 9 do not exist
          WorkshopId(30) -> Set.empty, WorkshopId(31) -> Set.empty, WorkshopId(32) -> Set(student1, student2), // TopicId(10)
          // workshops for topics 11, 12, 13 do not exist
          WorkshopId(42) -> Set.empty, WorkshopId(43) -> Set.empty, WorkshopId(44) -> Set.empty, // TopicId(14)
        )

        distributionAlgorithm(f.topics, workshopsWsRemoved)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "finds a distribution which respects the grade of a student and workshop" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 4)
        // reset grades of workshops 0, 6, 7, 8 to non-matching grades
        val workshopsGradesChanged = Set(0, 7, 8).foldLeft(f.workshops) { case (workshops, id) =>
          workshops.updatedWith(WorkshopId(id))(_.map {
            case (topicId, timeSlot, _, seats) => (topicId, timeSlot, Set(f.gradeNonMatching), seats)
          })
        }
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(
          student1 -> (f.grade, BiMap(
            TopicId(0) -> SelectionPriority(1), // for student's grade only available in 2nd and 3rd timeslot
            TopicId(1) -> SelectionPriority(2), // available for student's grade in all 3 timeslots
            TopicId(2) -> SelectionPriority(3), // not at all available for student's grade
          )),
        )
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set.empty, WorkshopId(1) -> Set(student1), WorkshopId(2) -> Set.empty, // TopicId(0)
          WorkshopId(3) -> Set(student1), WorkshopId(4) -> Set.empty, WorkshopId(5) -> Set.empty, // TopicId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set.empty, // TopicId(2)
          WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set(student1), // TopicId(3)
        )

        distributionAlgorithm(f.topics, workshopsGradesChanged)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

      "fails the distribution if no workshop with the right grade can be found" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 3)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.gradeNonMatching, BiMap.empty[TopicId, SelectionPriority]))

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics) shouldBe empty
      }

    }

  }

}
