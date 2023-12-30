package hcd.algorithm

import hcd.algorithm.Algorithm._
import hcd.models._
import io.cvbio.collection.mutable.bimap.BiMap
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.unused
import scala.util.Random

class AlgorithmSpec extends AnyWordSpec with Matchers {

  "Algorithm" should {

    trait FixtureWorkshops {
      def topics: Topics

      def workshops: Workshops

      def workshopSeats: WorkshopSeats

      // create a WorkshopComboCandidate from workshop ids with random SelectionPriority
      def workshopComboCandidate(wsIds: Set[Int]): WorkshopComboCandidate =
        BiMap.from(
          wsIds
            .map(WorkshopId)
            .map { workshopId =>
              val TopicTimeslot(topicId, timeSlot) = workshops(workshopId)
              val category = topics(topicId)
              workshopId -> WorkshopCandidate(topicId, timeSlot, category, SelectionPriority(Random.nextInt()))
            }
        )

      // create a WorkshopComboCandidate from workshop id and selection prio combos
      def workshopComboCandidate(wsIdSelPrios: BiMap[Int, Int]): WorkshopComboCandidate =
        wsIdSelPrios
          .map { case (wsId, selPrio) =>
            val workshopId = WorkshopId(wsId)
            val TopicTimeslot(topicId, timeSlot) = workshops(workshopId)
            val category = topics(topicId)
            workshopId -> WorkshopCandidate(topicId, timeSlot, category, SelectionPriority(selPrio))
          }

      // create workshop combos from workshop ids, taking the selection priority from matching workshops
      def workshopCombos(matchingWorkshops: MatchingWorkshops)(wsIdCombos: Set[Set[Int]]): Set[WorkshopCombo] =
        wsIdCombos.map(wsIdCombo =>
          BiMap.from(
            wsIdCombo
              .map(WorkshopId)
              .map { workshopId =>
                val category = topics(workshops(workshopId).topicId)
                val selectionPriority = matchingWorkshops(workshopId)
                workshopId -> PossibleWorkshop(category, selectionPriority)
              })
        )
    }

    def fixtureSymmetricWorkshopsFor(noTopics: Int, noSeats: Int): FixtureWorkshops = new FixtureWorkshops {
      // Inputs for model size
      private val timeSlots = Seq(FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)
      private val categories = Seq(Nutrition, Relaxation, Sports)
      private val noWorkshops = noTopics * timeSlots.size // all workshop topics are available on all timeslots

      // Generate all IDs
      private val topicIds = Range(0, noTopics).map(TopicId)
      private val workshopIds = Range(0, noWorkshops).map(WorkshopId)

      // Generate symmetric workshops:
      // workshop categories are equally distributed among topics
      // each workshop topic exists in all timeslot
      // no limits of workshop seats
      // categories alter n,n,n, r,r,r, s,s,s, n,n,n, ...
      // topicIds alter 0,0,0, 1,1,1, 2,2,2, 3,3,3, ...
      // timeslots alter f,s,t, f,s,t, f,s,t, f,s,t, ...
      override val topics: Topics = topicIds.map(topicId => topicId -> categories(topicId.id % categories.size)).toMap
      override val workshops: Workshops = BiMap.from(workshopIds.map(workshopId =>
        workshopId -> TopicTimeslot(
          TopicId(workshopId.id / timeSlots.size),
          timeSlots(workshopId.id % timeSlots.size)
        )
      ))
      override val workshopSeats: WorkshopSeats = workshopIds.map(_ -> Seats(noSeats)).toMap
    }

    def fixtureSymmetricWorkshops(noTopics: Int): FixtureWorkshops =
      fixtureSymmetricWorkshopsFor(noTopics, 20)

    trait FixtureFullDataModel extends FixtureWorkshops {
      // Inputs for model size
      private val noTopics = 50
      private val noStudents = 1000
      private val noSelectionsPerStudent = 6
      private val noSeats = 20

      private val underlyingWorkshops = fixtureSymmetricWorkshopsFor(noTopics, noSeats)
      override val topics: Topics = underlyingWorkshops.topics
      override val workshops: Workshops = underlyingWorkshops.workshops
      override val workshopSeats: WorkshopSeats = underlyingWorkshops.workshopSeats
      private lazy val studentIds: Set[StudentId] = Range(0, noStudents).toSet.map(StudentId)
      private lazy val selectionPriorities: Set[SelectionPriority] = Range.inclusive(1, noSelectionsPerStudent).toSet.map(SelectionPriority)

      // generate random workshop selections
      Random.setSeed(0L) // fix randomness during development
      lazy val studentsSelectedTopics: StudentsSelectedTopics = studentIds.map(
        _ -> BiMap.from(Random.shuffle(topics.keySet.toSeq).zip(selectionPriorities))
      ).toMap
    }

    def fixtureFullDataModel: FixtureFullDataModel = new FixtureFullDataModel {}

    "select MatchingWorkshops from SelectedTopics" in {
      val f = fixtureSymmetricWorkshops(noTopics = 4)
      val fut: SelectedTopics => MatchingWorkshops = matchingWorkshopsFromSelectedTopics(f.workshops)

      val selectedWorkshopTopics1: SelectedTopics = BiMap(
        TopicId(0) -> SelectionPriority(1),
        TopicId(1) -> SelectionPriority(2),
      )
      val selectedWorkshopTopics2: SelectedTopics = BiMap(
        TopicId(3) -> SelectionPriority(5),
        TopicId(2) -> SelectionPriority(6),
      )
      val selectedWorkshopTopics3: SelectedTopics = BiMap(
        TopicId(4) -> SelectionPriority(5), // non-existing workshop topic
      )
      val expectedMatchingWorkshops1: MatchingWorkshops = Map(
        WorkshopId(0) -> SelectionPriority(1), WorkshopId(1) -> SelectionPriority(1), WorkshopId(2) -> SelectionPriority(1), // TopicId(0)
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // TopicId(1)
      )
      val expectedMatchingWorkshops2: MatchingWorkshops = Map(
        WorkshopId(9) -> SelectionPriority(5), WorkshopId(10) -> SelectionPriority(5), WorkshopId(11) -> SelectionPriority(5), // TopicId(3)
        WorkshopId(6) -> SelectionPriority(6), WorkshopId(7) -> SelectionPriority(6), WorkshopId(8) -> SelectionPriority(6), // TopicId(2)
      )

      fut(selectedWorkshopTopics1) should contain theSameElementsAs expectedMatchingWorkshops1
      fut(selectedWorkshopTopics2) should contain theSameElementsAs expectedMatchingWorkshops2
      fut(selectedWorkshopTopics3) shouldBe empty
    }

    "select StudentsMatchingWorkshops from StudentsSelectedTopics" in {
      val f = fixtureSymmetricWorkshops(noTopics = 19)
      val fut: StudentsSelectedTopics => StudentsMatchingWorkshops = studentsMatchingWorkshopsFromStudentSelectedTopics(f.workshops)

      val student1 = StudentId(5)
      val student2 = StudentId(42)
      val student3 = StudentId(-1)
      val studentWorkshopSelections: StudentsSelectedTopics = Map(
        student1 -> BiMap(
          TopicId(14) -> SelectionPriority(7),
          TopicId(18) -> SelectionPriority(9),
          TopicId(10) -> SelectionPriority(5),
        ),
        student2 -> BiMap(
          TopicId(3) -> SelectionPriority(6),
          TopicId(1) -> SelectionPriority(2),
          TopicId(2) -> SelectionPriority(4),
        ),
        student3 -> BiMap(
          TopicId(19) -> SelectionPriority(-2), // non-existing workshop topic
        ),
      )
      val expectedStudent1MatchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(42) -> SelectionPriority(7), WorkshopId(43) -> SelectionPriority(7), WorkshopId(44) -> SelectionPriority(7), // TopicId(14)
        WorkshopId(54) -> SelectionPriority(9), WorkshopId(55) -> SelectionPriority(9), WorkshopId(56) -> SelectionPriority(9), // TopicId(18)
        WorkshopId(30) -> SelectionPriority(5), WorkshopId(31) -> SelectionPriority(5), WorkshopId(32) -> SelectionPriority(5), // TopicId(10)
      )
      val expectedStudent2MatchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(9) -> SelectionPriority(6), WorkshopId(10) -> SelectionPriority(6), WorkshopId(11) -> SelectionPriority(6), // TopicId(3)
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // TopicId(1)
        WorkshopId(6) -> SelectionPriority(4), WorkshopId(7) -> SelectionPriority(4), WorkshopId(8) -> SelectionPriority(4), // TopicId(2)
      )
      val expectedStudentsMatchingWorkshops = Map(
        student1 -> expectedStudent1MatchingWorkshops,
        student2 -> expectedStudent2MatchingWorkshops,
        student3 -> Map.empty,
      )

      fut(studentWorkshopSelections) should contain theSameElementsAs expectedStudentsMatchingWorkshops
    }

    "filter a WorkshopComboCandidate via hasDistinctTopicIds" in {
      val f = fixtureSymmetricWorkshops(noTopics = 2)

      val workshopComboCandidate1 = f.workshopComboCandidate(Set(1, 2))
      val workshopComboCandidate2 = f.workshopComboCandidate(Set(0, 2, 3))
      // artificial combination, with the symmetric fixture such choice could not have happened
      val workshopComboCandidate3 = f.workshopComboCandidate(Set(0, 3))
      val workshopComboCandidate4 = f.workshopComboCandidate(Set(0, 4))
      val workshopComboCandidate5: WorkshopComboCandidate = BiMap.empty

      hasDistinctTopicIds(workshopComboCandidate1) shouldEqual false
      hasDistinctTopicIds(workshopComboCandidate2) shouldEqual false
      hasDistinctTopicIds(workshopComboCandidate3) shouldEqual true
      hasDistinctTopicIds(workshopComboCandidate4) shouldEqual true
      hasDistinctTopicIds(workshopComboCandidate5) shouldEqual false
    }

    "filter a WorkshopComboCandidate via hasDistinctTimeslots" in {
      val f = fixtureSymmetricWorkshops(noTopics = 3)

      val workshopComboCandidate1 = f.workshopComboCandidate(Set(0, 1, 2, 3, 4, 5))
      val workshopComboCandidate2 = f.workshopComboCandidate(Set(0, 1, 5, 7))
      val workshopComboCandidate3 = f.workshopComboCandidate(Set(0, 5))
      val workshopComboCandidate4 = f.workshopComboCandidate(Set(1, 5))
      val workshopComboCandidate5 = f.workshopComboCandidate(Set(2, 5))
      val workshopComboCandidate6 = f.workshopComboCandidate(Set(0, 8, 4))
      val workshopComboCandidate7: WorkshopComboCandidate = BiMap.empty

      hasDistinctTimeslots(workshopComboCandidate1) shouldEqual false
      hasDistinctTimeslots(workshopComboCandidate2) shouldEqual false
      hasDistinctTimeslots(workshopComboCandidate3) shouldEqual true
      hasDistinctTimeslots(workshopComboCandidate4) shouldEqual true
      hasDistinctTimeslots(workshopComboCandidate5) shouldEqual false
      hasDistinctTimeslots(workshopComboCandidate6) shouldEqual true
      hasDistinctTimeslots(workshopComboCandidate7) shouldEqual false
    }

    "filter a WorkshopComboCandidate via hasVaryingCategories" in {
      val f = fixtureSymmetricWorkshops(noTopics = 6)

      val workshopComboCandidate1 = f.workshopComboCandidate(Set(0))
      val workshopComboCandidate2 = f.workshopComboCandidate(Set(3))
      val workshopComboCandidate3 = f.workshopComboCandidate(Set(6))
      val workshopComboCandidate4 = f.workshopComboCandidate(Set(0, 9))
      val workshopComboCandidate5 = f.workshopComboCandidate(Set(4, 13))
      val workshopComboCandidate6 = f.workshopComboCandidate(Set(6, 15))
      val workshopComboCandidate7 = f.workshopComboCandidate(Set(6, 7, 8))
      val workshopComboCandidate8: WorkshopComboCandidate = BiMap.empty

      hasVaryingCategories(workshopComboCandidate1) shouldEqual false
      hasVaryingCategories(workshopComboCandidate2) shouldEqual false
      hasVaryingCategories(workshopComboCandidate3) shouldEqual true
      hasVaryingCategories(workshopComboCandidate4) shouldEqual false
      hasVaryingCategories(workshopComboCandidate5) shouldEqual false
      hasVaryingCategories(workshopComboCandidate6) shouldEqual true
      hasVaryingCategories(workshopComboCandidate7) shouldEqual true
      hasVaryingCategories(workshopComboCandidate8) shouldEqual false
    }

    "filter a WorkshopComboCandidate via hasSufficientSelectionPriority" in {
      val f = fixtureSymmetricWorkshops(noTopics = 4)

      val workshopComboCandidate1 = f.workshopComboCandidate(BiMap(10 -> 1, 11 -> 2))
      val workshopComboCandidate2 = f.workshopComboCandidate(BiMap(10 -> 2, 11 -> 4))
      val workshopComboCandidate3 = f.workshopComboCandidate(BiMap(10 -> 3, 11 -> 5))
      val workshopComboCandidate4 = f.workshopComboCandidate(BiMap(10 -> 4, 11 -> 6))
      val workshopComboCandidate5: WorkshopComboCandidate = BiMap.empty

      hasSufficientSelectionPriority(workshopComboCandidate1) shouldEqual true
      hasSufficientSelectionPriority(workshopComboCandidate2) shouldEqual true
      hasSufficientSelectionPriority(workshopComboCandidate3) shouldEqual true
      hasSufficientSelectionPriority(workshopComboCandidate4) shouldEqual false
      hasSufficientSelectionPriority(workshopComboCandidate5) shouldEqual false
    }

    "generate all possible combinations of workshops from given workshops, comboSize, and matching workshops, with regards to topicId and timeslots" in {
      val f = fixtureSymmetricWorkshops(noTopics = 3)

      val matchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(0) -> SelectionPriority(1), WorkshopId(1) -> SelectionPriority(1), WorkshopId(2) -> SelectionPriority(1), // TopicId(0)
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // TopicId(1)
        WorkshopId(6) -> SelectionPriority(3), WorkshopId(7) -> SelectionPriority(3), WorkshopId(8) -> SelectionPriority(3), // TopicId(2)
      )
      val alwaysTrue: Any => Boolean = _ => true
      val genCombos: Set[Set[Int]] => Set[WorkshopCombo] = f.workshopCombos(matchingWorkshops)
      val expectedCombos1 = genCombos(Set(
        Set(0),
        Set(1),
        Set(2),
        Set(3),
        Set(4),
        Set(5),
        Set(6),
        Set(7),
        Set(8),
      ))
      val expectedCombos2 = genCombos(Set(
        Set(0, 4),
        Set(0, 5),
        Set(0, 7),
        Set(0, 8),
        Set(1, 3),
        Set(1, 5),
        Set(1, 6),
        Set(1, 8),
        Set(2, 3),
        Set(2, 4),
        Set(2, 6),
        Set(2, 7),
        Set(3, 7),
        Set(3, 8),
        Set(4, 6),
        Set(4, 8),
        Set(5, 6),
        Set(5, 7),
      ))
      val expectedCombos3 = genCombos(Set(
        Set(0, 4, 8),
        Set(0, 5, 7),
        Set(1, 3, 8),
        Set(1, 5, 6),
        Set(2, 3, 7),
        Set(2, 4, 6),
      ))
      val expectedCombos4 = Seq.empty // because a combo of 4 will always overlap on timeslots

      val workshopCombos1 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 1, alwaysTrue)(matchingWorkshops)
      val workshopCombos2 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 2, alwaysTrue)(matchingWorkshops)
      val workshopCombos3 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 3, alwaysTrue)(matchingWorkshops)
      val workshopCombos4 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 4, alwaysTrue)(matchingWorkshops)

      workshopCombos1 should contain theSameElementsAs expectedCombos1
      workshopCombos2 should contain theSameElementsAs expectedCombos2
      workshopCombos3 should contain theSameElementsAs expectedCombos3
      workshopCombos4 should contain theSameElementsAs expectedCombos4
    }

    "generate all possible combinations of workshops from given workshops, comboSize, and matching workshops, also with regards to varying categories" in {
      val f = fixtureSymmetricWorkshops(noTopics = 4)

      val matchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(0) -> SelectionPriority(1), WorkshopId(1) -> SelectionPriority(1), WorkshopId(2) -> SelectionPriority(1), // TopicId(0) nutrition
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // TopicId(1) relaxation
        WorkshopId(9) -> SelectionPriority(3), WorkshopId(10) -> SelectionPriority(3), WorkshopId(11) -> SelectionPriority(3), // TopicId(3) nutrition again
      )
      val genCombos: Set[Set[Int]] => Set[WorkshopCombo] = f.workshopCombos(matchingWorkshops)
      val expectedCombos1 = Set.empty // because with a combo size of 1 there is no variance in categories
      val expectedCombos2 = genCombos(Set( // combos with only nutrition or only relaxation are excluded
        Set(0, 4),
        Set(0, 5),
        Set(1, 3),
        Set(1, 5),
        Set(2, 3),
        Set(2, 4),
        Set(3, 10),
        Set(3, 11),
        Set(4, 9),
        Set(4, 11),
        Set(5, 9),
        Set(5, 10),
      ))
      val expectedCombos3 = genCombos(Set(
        Set(0, 4, 11),
        Set(0, 5, 10),
        Set(1, 3, 11),
        Set(1, 5, 9),
        Set(2, 3, 10),
        Set(2, 4, 9),
      ))
      val expectedCombos4 = Seq.empty // because a combo of 4 will always overlap on timeslots

      val workshopCombos1 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 1, hasVaryingCategories)(matchingWorkshops)
      val workshopCombos2 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 2, hasVaryingCategories)(matchingWorkshops)
      val workshopCombos3 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 3, hasVaryingCategories)(matchingWorkshops)
      val workshopCombos4 = generateWorkshopCombos(f.workshops, f.topics, comboSize = 4, hasVaryingCategories)(matchingWorkshops)

      workshopCombos1 should contain theSameElementsAs expectedCombos1
      workshopCombos2 should contain theSameElementsAs expectedCombos2
      workshopCombos3 should contain theSameElementsAs expectedCombos3
      workshopCombos4 should contain theSameElementsAs expectedCombos4
    }

    "generate all possible combinations of workshops for students from given workshops, comboSize, and the selected workshop topics of the students" in {
      val f = fixtureSymmetricWorkshops(noTopics = 7)

      val comboSize = 3
      val student1 = StudentId(11)
      val student2 = StudentId(12)
      val student3 = StudentId(13)
      val student4 = StudentId(14)
      val studentsSelectedTopics: StudentsSelectedTopics = Map(
        student1 -> BiMap(
          TopicId(0) -> SelectionPriority(1),
          TopicId(1) -> SelectionPriority(2),
          TopicId(2) -> SelectionPriority(3),
        ),
        student2 -> BiMap(
          TopicId(1) -> SelectionPriority(3),
          TopicId(3) -> SelectionPriority(5),
          TopicId(5) -> SelectionPriority(4),
        ),
        student3 -> BiMap( // actually an illegal choice, as all 3 workshops are of category nutrition
          TopicId(0) -> SelectionPriority(1), // nutrition
          TopicId(3) -> SelectionPriority(2), // nutrition
          TopicId(6) -> SelectionPriority(3), // nutrition
        ),
        student4 -> BiMap( // no selection priority 1, 2, 3
          TopicId(1) -> SelectionPriority(6),
          TopicId(3) -> SelectionPriority(5),
          TopicId(5) -> SelectionPriority(4),
        ),
      )
      val expectedWsIdCombos1 = Set(
        Set(0, 4, 8),
        Set(0, 5, 7),
        Set(1, 3, 8),
        Set(1, 5, 6),
        Set(2, 3, 7),
        Set(2, 4, 6),
      )
      val expectedWsIdCombos2 = Set(
        Set(3, 10, 17),
        Set(3, 11, 16),
        Set(4, 9, 17),
        Set(4, 11, 15),
        Set(5, 9, 16),
        Set(5, 10, 15),
      )
      val expectedStudentsWsIdCombos = Map(
        student1 -> expectedWsIdCombos1,
        student2 -> expectedWsIdCombos2,
        student3 -> Set.empty, // as all 3 workshops are of category nutrition
        student4 -> Set.empty, // as there is no workshops with selection priority 1, 2, 3
      )
      val expectedStudentsWorkshopCombos = expectedStudentsWsIdCombos.map { case (studentId, expectedWsIdCombos) =>
        val expectedWorkshopCombos = expectedWsIdCombos.map(expectedWsIdCombo =>
          expectedWsIdCombo
            .map(WorkshopId)
            .map { workshopId =>
              val workshop = f.workshops(workshopId)
              val category = f.topics(workshop.topicId)
              val selectionPriority = studentsSelectedTopics(studentId).valueFor(workshop.topicId).get
              workshopId -> PossibleWorkshop(category, selectionPriority)
            }
            .toMap)
        studentId -> expectedWorkshopCombos
      }

      val studentsWorkshopCombos = generateStudentsWorkshopCombos(f.workshops, f.topics, comboSize)(studentsSelectedTopics)

      studentsWorkshopCombos should contain theSameElementsAs expectedStudentsWorkshopCombos
    }

    "update free seats" in {
      val f = fixtureSymmetricWorkshops(2)

      val workshopIds1 = Seq(WorkshopId(1), WorkshopId(3), WorkshopId(5))
      val workshopIds2 = Seq.empty
      val expectedFreeWorkshopSeats1 = Map(
        WorkshopId(0) -> Seats(20), WorkshopId(1) -> Seats(19), WorkshopId(2) -> Seats(20), // TopicId(0)
        WorkshopId(3) -> Seats(19), WorkshopId(4) -> Seats(20), WorkshopId(5) -> Seats(19), // TopicId(1)
      )
      val expectedFreeWorkshopSeats2 = Map(
        WorkshopId(0) -> Seats(20), WorkshopId(1) -> Seats(20), WorkshopId(2) -> Seats(20), // TopicId(0)
        WorkshopId(3) -> Seats(20), WorkshopId(4) -> Seats(20), WorkshopId(5) -> Seats(20), // TopicId(1)
      )

      updateFreeWorkshopSeats(f.workshopSeats, workshopIds1) should contain theSameElementsAs expectedFreeWorkshopSeats1
      updateFreeWorkshopSeats(f.workshopSeats, workshopIds2) should contain theSameElementsAs expectedFreeWorkshopSeats2
    }

    "provide a method to distribute students to workshops" which {

      "yields an empty distribution if no selections were made" in {
        val f = fixtureSymmetricWorkshops(noTopics = 1)

        val comboSize = 3
        val studentsSelectedTopics: StudentsSelectedTopics = Map.empty
        val expectedDistribution = (f.workshops.view.mapValues(_ => Set.empty).toMap, Metric(0), f.workshopSeats)

        distributeStudentsToWorkshops(f.workshops, f.topics, f.workshopSeats, comboSize)(studentsSelectedTopics) shouldEqual expectedDistribution
      }

      "yields a valid distribution for a single student with combo size 3" in {
        val f = fixtureSymmetricWorkshops(noTopics = 4)

        val comboSize = 3
        val student1 = StudentId(1)
        val studentWorkshopSelections: StudentsSelectedTopics = Map(
          student1 -> BiMap(
            TopicId(0) -> SelectionPriority(1),
            TopicId(1) -> SelectionPriority(2),
            TopicId(2) -> SelectionPriority(3),
          ),
        )
        // assumes that the algorithm orders the input so that the result is stable
        val expectedResult = (
          Map(
            WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
            WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
            WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // TopicId(2)
            WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set.empty, // TopicId(3)
          ),
          Metric(6),
          Map(
            WorkshopId(0) -> Seats(19), WorkshopId(1) -> Seats(20), WorkshopId(2) -> Seats(20), // TopicId(0)
            WorkshopId(3) -> Seats(20), WorkshopId(4) -> Seats(19), WorkshopId(5) -> Seats(20), // TopicId(1)
            WorkshopId(6) -> Seats(20), WorkshopId(7) -> Seats(20), WorkshopId(8) -> Seats(19), // TopicId(2)
            WorkshopId(9) -> Seats(20), WorkshopId(10) -> Seats(20), WorkshopId(11) -> Seats(20), // TopicId(3)
          )
        )

        distributeStudentsToWorkshops(f.workshops, f.topics, f.workshopSeats, comboSize)(studentWorkshopSelections) shouldEqual expectedResult
      }

      "yields a valid distribution for two students with combo size 2" in {
        val f = fixtureSymmetricWorkshops(noTopics = 4)

        val comboSize = 2
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentWorkshopSelections: StudentsSelectedTopics = Map(
          student1 -> BiMap(
            TopicId(0) -> SelectionPriority(1),
            TopicId(1) -> SelectionPriority(2),
            TopicId(2) -> SelectionPriority(3),
          ),
          student2 -> BiMap(
            TopicId(0) -> SelectionPriority(2),
            TopicId(2) -> SelectionPriority(4),
            TopicId(3) -> SelectionPriority(3),
          ),
        )
        // assumes that the algorithm orders the input so that the result is stable
        val expectedResult = (
          Map(
            WorkshopId(0) -> Set(student1, student2), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
            WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // TopicId(1)
            WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set(student2), WorkshopId(8) -> Set.empty, // TopicId(2)
            WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set.empty, // TopicId(3)
          ),
          Metric(9),
          Map(
            WorkshopId(0) -> Seats(18), WorkshopId(1) -> Seats(20), WorkshopId(2) -> Seats(20), // TopicId(0)
            WorkshopId(3) -> Seats(20), WorkshopId(4) -> Seats(19), WorkshopId(5) -> Seats(20), // TopicId(1)
            WorkshopId(6) -> Seats(20), WorkshopId(7) -> Seats(19), WorkshopId(8) -> Seats(20), // TopicId(2)
            WorkshopId(9) -> Seats(20), WorkshopId(10) -> Seats(20), WorkshopId(11) -> Seats(20), // TopicId(3)
          )
        )

        distributeStudentsToWorkshops(f.workshops, f.topics, f.workshopSeats, comboSize)(studentWorkshopSelections) shouldEqual expectedResult
      }

      "yields a distribution despite one student having an illegal selection" in {
        // Such situation should in production be checked and rejected before entering the distribution.
        // However, during tests with arbitrary input data this could happen and thus the distribution algorithm
        // must handle it gracefully, i.e. ignore the student with no possible workshop combos.
        val f = fixtureSymmetricWorkshops(noTopics = 4)

        val comboSize = 2
        val student1 = StudentId(1)
        val student2 = StudentId(2)
        val studentWorkshopSelections: StudentsSelectedTopics = Map(
          student1 -> BiMap( // actually an illegal choice, as both workshops are of category nutrition
            TopicId(0) -> SelectionPriority(1), // nutrition
            TopicId(3) -> SelectionPriority(2), // nutrition
          ),
          student2 -> BiMap(
            TopicId(0) -> SelectionPriority(2),
            TopicId(2) -> SelectionPriority(4),
            TopicId(3) -> SelectionPriority(3),
          ),
        )
        // assumes that the algorithm orders the input so that the result is stable
        val expectedResult = (
          Map(
            WorkshopId(0) -> Set(student2), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
            WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set.empty, WorkshopId(5) -> Set.empty, // TopicId(1)
            WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set(student2), WorkshopId(8) -> Set.empty, // TopicId(2)
            WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set.empty, // TopicId(3)
          ),
          Metric(6),
          Map(
            WorkshopId(0) -> Seats(19), WorkshopId(1) -> Seats(20), WorkshopId(2) -> Seats(20), // TopicId(0)
            WorkshopId(3) -> Seats(20), WorkshopId(4) -> Seats(20), WorkshopId(5) -> Seats(20), // TopicId(1)
            WorkshopId(6) -> Seats(20), WorkshopId(7) -> Seats(19), WorkshopId(8) -> Seats(20), // TopicId(2)
            WorkshopId(9) -> Seats(20), WorkshopId(10) -> Seats(20), WorkshopId(11) -> Seats(20), // TopicId(3)
          )
        )

        distributeStudentsToWorkshops(f.workshops, f.topics, f.workshopSeats, comboSize)(studentWorkshopSelections) shouldEqual expectedResult
      }

    }

    "build test data correctly and optionally print it" in {
      val f = fixtureFullDataModel

      f.topics(TopicId(0)) shouldEqual Nutrition
      f.topics(TopicId(1)) shouldEqual Relaxation
      f.topics(TopicId(2)) shouldEqual Sports
      f.workshops(WorkshopId(0)) shouldEqual TopicTimeslot(TopicId(0), FirstTimeSlot)
      f.workshops(WorkshopId(4)) shouldEqual TopicTimeslot(TopicId(1), SecondTimeSlot)
      f.workshops(WorkshopId(8)) shouldEqual TopicTimeslot(TopicId(2), ThirdTimeSlot)
      f.workshopSeats(WorkshopId(0)).n shouldEqual 20
      f.workshopSeats(WorkshopId(149)).n shouldEqual 20

      // print workshops ordered by id
      //f.workshops.toSeq.sortBy(_._1.id).foreach(println)

      // print students' selected workshop topics ordered by student id
      //f.studentsSelectedTopics.toSeq.sortBy(_._1.id).foreach(println)

      // print students' matching workshops from their selected workshop topics for full model
      @unused // may be unused, depending on whether the model is printed out our not
      lazy val studentsMatchingWorkshops = studentsMatchingWorkshopsFromStudentSelectedTopics(f.workshops)(f.studentsSelectedTopics)
      //studentsMatchingWorkshops.toSeq.sortBy(_._1.id).foreach(t => println(t._1, collection.SortedMap.from(t._2)(Ordering.by(_.id))))

      // print students' workshop combos for full model
      // per student there are 96 possible combos to chose 3 out of 6 workshops
      // print those for the first 2 students
      @unused // may be unused, depending on whether the model is printed out our not
      lazy val studentsWorkshopCombos = generateStudentsWorkshopCombos(f.workshops, f.topics, comboSize = 3)(f.studentsSelectedTopics)
      //println(studentsWorkshopCombos.view.filterKeys(_.id < 2).toMap)

      // print distributeStudentsToWorkshops for full model
      lazy val (workshopAssignments, metric, leftFreeWorkshopSeats) = distributeStudentsToWorkshops(f.workshops, f.topics, f.workshopSeats, comboSize = 3)(f.studentsSelectedTopics)
      if (System.getProperty("DistributeStudentsToWorkshops", "false").toBooleanOption.getOrElse(false))
        println(workshopAssignments, metric, leftFreeWorkshopSeats)
    }

  }

}
