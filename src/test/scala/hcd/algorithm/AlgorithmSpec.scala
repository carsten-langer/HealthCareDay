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
      def workshops: Workshops

      // create a WorkshopComboCandidate from workshop ids with random SelectionPriority
      def workshopComboCandidate(wsIds: Set[Int]): WorkshopComboCandidate =
        wsIds
          .map(WorkshopId)
          .map(wsId => (wsId, (workshops(wsId), SelectionPriority(Random.nextInt()))))
          .toMap

      // create workshop combos from workshop ids, taking the selection priority from matching workshops
      def workshopCombos(matchingWorkshops: MatchingWorkshops)(wsIdCombos: Set[Set[Int]]): Set[WorkshopCombo] =
        wsIdCombos.map(wsIdCombo =>
          wsIdCombo
            .map(WorkshopId)
            .map { workshopId =>
              val category = workshops(workshopId).category
              val selectionPriority = matchingWorkshops(workshopId)
              workshopId -> PossibleWorkshop(category, selectionPriority)
            }
            .toMap
        )
    }

    def fixtureSymmetricWorkshopsFor(noWorkshopChoices: Int, noSeats: Int): FixtureWorkshops = new FixtureWorkshops {
      // Inputs for model size
      private val timeSlots = Seq(FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)
      private val categories = Seq(Health, Relaxation, Sports)
      private val noWorkshops = noWorkshopChoices * timeSlots.size // all workshops are available on all timeslots

      // Generate all IDs
      private val workshopIds = Range(0, noWorkshops).map(WorkshopId)

      // Generate symmetric workshops:
      // each workshop choice exists in all timeslot
      // workshop categories are equally distributed
      // no limits of workshop seats
      override val workshops: Workshops = workshopIds.map(workshopId =>
        (workshopId, Workshop(
          categories(workshopId.id / 3 % 3), // categories alter h,h,h, r,r,r, s,s,s, h,h,h, ...
          WorkshopChoiceId(workshopId.id / 3), // choiceIds alter 0,0,0, 1,1,1, 2,2,2, 3,3,3, ...
          timeSlots(workshopId.id % 3), // timeslots alter f,s,t, f,s,t, f,s,t, f,s,t, ...
          noSeats
        ))
      ).toMap
    }

    def fixtureSymmetricWorkshops(noWorkshopChoices: Int): FixtureWorkshops =
      fixtureSymmetricWorkshopsFor(noWorkshopChoices, 20)

    trait FixtureFullDataModel extends FixtureWorkshops {
      // Inputs for model size
      private val noWorkshopChoices = 50
      private val noStudents = 1000
      private val noSelectionsPerStudent = 6
      private val noSeats = 20

      override val workshops: Workshops = fixtureSymmetricWorkshopsFor(noWorkshopChoices, noSeats).workshops
      private lazy val workshopChoiceIds: Set[WorkshopChoiceId] = Range(0, noWorkshopChoices).toSet.map(WorkshopChoiceId)
      private lazy val studentIds: Set[StudentId] = Range(0, noStudents).toSet.map(StudentId)
      private lazy val selectionPriorities: Set[SelectionPriority] = Range.inclusive(1, noSelectionsPerStudent).toSet.map(SelectionPriority)

      // generate random workshop selections
      Random.setSeed(0L) // fix randomness during development
      lazy val studentsSelectedWorkshopChoices: StudentsSelectedWorkshopChoices = studentIds.map(
        (_, BiMap.from(selectionPriorities.zip(Random.shuffle(workshopChoiceIds.toSeq))))
      ).toMap
    }

    def fixtureFullDataModel: FixtureFullDataModel = new FixtureFullDataModel {}

    "build test data correctly and optionally print it" in {
      val f = fixtureFullDataModel

      f.workshops(WorkshopId(0)) shouldEqual Workshop(Health, WorkshopChoiceId(0), FirstTimeSlot, 20)
      f.workshops(WorkshopId(4)) shouldEqual Workshop(Relaxation, WorkshopChoiceId(1), SecondTimeSlot, 20)
      f.workshops(WorkshopId(8)) shouldEqual Workshop(Sports, WorkshopChoiceId(2), ThirdTimeSlot, 20)

      // print workshops ordered by id
      //f.workshops.toSeq.sortBy(_._1.id).foreach(println)

      // print students' selected workshop choices ordered by student id
      //f.studentsSelectedWorkshopChoices.toSeq.sortBy(_._1.id).foreach(println)

      // print students' matching workshops from their selected workshop choices for full model
      @unused // may be unused, depending on whether the model is printed out our not
      lazy val studentsMatchingWorkshops = studentsMatchingWorkshopsFromStudentSelectedWorkshopChoices(f.workshops)(f.studentsSelectedWorkshopChoices)
      //studentsMatchingWorkshops.toSeq.sortBy(_._1.id).foreach(t => println(t._1, collection.SortedMap.from(t._2)(Ordering.by(_.id))))

      // print students' workshop combos for full model
      // per student there are 96 possible combos to chose 3 out of 6 workshops
      // print those for the first 2 students
      @unused // may be unused, depending on whether the model is printed out our not
      lazy val studentsWorkshopCombos = generateStudentsWorkshopCombos(f.workshops, comboSize = 3)(f.studentsSelectedWorkshopChoices)
      //println(studentsWorkshopCombos.view.filterKeys(_.id < 2).toMap)

      // print distributeStudentsToWorkshops for full model
      lazy val (workshopAssignments, metric) = distributeStudentsToWorkshops(f.workshops, comboSize = 3)(f.studentsSelectedWorkshopChoices)
      if (System.getProperty("DistributeStudentsToWorkshops", "false").toBooleanOption.getOrElse(false))
        println(workshopAssignments, metric)
    }

    "select MatchingWorkshops from SelectedWorkshopChoices" in {
      val f = fixtureSymmetricWorkshops(4)
      val fut: SelectedWorkshopChoices => MatchingWorkshops = matchingWorkshopsFromSelectedWorkshopChoice(f.workshops)

      val selectedWorkshopChoices1: SelectedWorkshopChoices = BiMap(
        SelectionPriority(1) -> WorkshopChoiceId(0),
        SelectionPriority(2) -> WorkshopChoiceId(1),
      )
      val selectedWorkshopChoices2: SelectedWorkshopChoices = BiMap(
        SelectionPriority(5) -> WorkshopChoiceId(3),
        SelectionPriority(6) -> WorkshopChoiceId(2),
      )
      val selectedWorkshopChoices3: SelectedWorkshopChoices = BiMap(
        SelectionPriority(5) -> WorkshopChoiceId(4), // non-existing workshop choice
      )
      val expectedMatchingWorkshops1: MatchingWorkshops = Map(
        WorkshopId(0) -> SelectionPriority(1), WorkshopId(1) -> SelectionPriority(1), WorkshopId(2) -> SelectionPriority(1), // WorkshopChoiceId(0)
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // WorkshopChoiceId(1)
      )
      val expectedMatchingWorkshops2: MatchingWorkshops = Map(
        WorkshopId(9) -> SelectionPriority(5), WorkshopId(10) -> SelectionPriority(5), WorkshopId(11) -> SelectionPriority(5), // WorkshopChoiceId(3)
        WorkshopId(6) -> SelectionPriority(6), WorkshopId(7) -> SelectionPriority(6), WorkshopId(8) -> SelectionPriority(6), // WorkshopChoiceId(2)
      )

      fut(selectedWorkshopChoices1) should contain theSameElementsAs expectedMatchingWorkshops1
      fut(selectedWorkshopChoices2) should contain theSameElementsAs expectedMatchingWorkshops2
      fut(selectedWorkshopChoices3) shouldBe empty
    }

    "select StudentsMatchingWorkshops from StudentsSelectedWorkshopChoices" in {
      val f = fixtureSymmetricWorkshops(19)
      val fut: StudentsSelectedWorkshopChoices => StudentsMatchingWorkshops = studentsMatchingWorkshopsFromStudentSelectedWorkshopChoices(f.workshops)

      val student1 = StudentId(5)
      val student2 = StudentId(42)
      val student3 = StudentId(-1)
      val studentWorkshopSelections: StudentsSelectedWorkshopChoices = Map(
        student1 -> BiMap(
          SelectionPriority(7) -> WorkshopChoiceId(14),
          SelectionPriority(9) -> WorkshopChoiceId(18),
          SelectionPriority(5) -> WorkshopChoiceId(10),
        ),
        student2 -> BiMap(
          SelectionPriority(6) -> WorkshopChoiceId(3),
          SelectionPriority(2) -> WorkshopChoiceId(1),
          SelectionPriority(4) -> WorkshopChoiceId(2),
        ),
        student3 -> BiMap(
          SelectionPriority(-2) -> WorkshopChoiceId(19), // non-existing workshop choice
        ),
      )
      val expectedStudent1MatchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(42) -> SelectionPriority(7), WorkshopId(43) -> SelectionPriority(7), WorkshopId(44) -> SelectionPriority(7), // WorkshopChoiceId(14)
        WorkshopId(54) -> SelectionPriority(9), WorkshopId(55) -> SelectionPriority(9), WorkshopId(56) -> SelectionPriority(9), // WorkshopChoiceId(18)
        WorkshopId(30) -> SelectionPriority(5), WorkshopId(31) -> SelectionPriority(5), WorkshopId(32) -> SelectionPriority(5), // WorkshopChoiceId(10)
      )
      val expectedStudent2MatchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(9) -> SelectionPriority(6), WorkshopId(10) -> SelectionPriority(6), WorkshopId(11) -> SelectionPriority(6), // WorkshopChoiceId(3)
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // WorkshopChoiceId(1)
        WorkshopId(6) -> SelectionPriority(4), WorkshopId(7) -> SelectionPriority(4), WorkshopId(8) -> SelectionPriority(4), // WorkshopChoiceId(2)
      )
      val expectedStudentsMatchingWorkshops = Map(
        student1 -> expectedStudent1MatchingWorkshops,
        student2 -> expectedStudent2MatchingWorkshops,
        student3 -> Map.empty,
      )

      fut(studentWorkshopSelections) should contain theSameElementsAs expectedStudentsMatchingWorkshops
    }

    "filter a WorkshopComboCandidate via hasDistinctChoiceIds" in {
      val f = fixtureSymmetricWorkshops(2)

      val workshopComboCandidate1 = f.workshopComboCandidate(Set(1, 2))
      val workshopComboCandidate2 = f.workshopComboCandidate(Set(0, 2, 3))
      // artificial combination, with the symmetric fixture such choice could not have happened
      val workshopComboCandidate3 = f.workshopComboCandidate(Set(0, 3))
      val workshopComboCandidate4 = f.workshopComboCandidate(Set(0, 4))
      val workshopComboCandidate5: WorkshopComboCandidate = Map.empty

      hasDistinctChoiceIds(workshopComboCandidate1) shouldEqual false
      hasDistinctChoiceIds(workshopComboCandidate2) shouldEqual false
      hasDistinctChoiceIds(workshopComboCandidate3) shouldEqual true
      hasDistinctChoiceIds(workshopComboCandidate4) shouldEqual true
      hasDistinctChoiceIds(workshopComboCandidate5) shouldEqual false
    }

    "filter a WorkshopComboCandidate via hasDistinctTimeslots" in {
      val f = fixtureSymmetricWorkshops(3)

      val workshopComboCandidate1 = f.workshopComboCandidate(Set(0, 1, 2, 3, 4, 5))
      val workshopComboCandidate2 = f.workshopComboCandidate(Set(0, 1, 5, 7))
      val workshopComboCandidate3 = f.workshopComboCandidate(Set(0, 5))
      val workshopComboCandidate4 = f.workshopComboCandidate(Set(1, 5))
      val workshopComboCandidate5 = f.workshopComboCandidate(Set(2, 5))
      val workshopComboCandidate6 = f.workshopComboCandidate(Set(0, 8, 4))
      val workshopComboCandidate7: WorkshopComboCandidate = Map.empty

      hasDistinctTimeslots(workshopComboCandidate1) shouldEqual false
      hasDistinctTimeslots(workshopComboCandidate2) shouldEqual false
      hasDistinctTimeslots(workshopComboCandidate3) shouldEqual true
      hasDistinctTimeslots(workshopComboCandidate4) shouldEqual true
      hasDistinctTimeslots(workshopComboCandidate5) shouldEqual false
      hasDistinctTimeslots(workshopComboCandidate6) shouldEqual true
      hasDistinctTimeslots(workshopComboCandidate7) shouldEqual false
    }

    "filter a WorkshopComboCandidate via hasVaryingCategories" in {
      val f = fixtureSymmetricWorkshops(6)

      val workshopComboCandidate1 = f.workshopComboCandidate(Set(0))
      val workshopComboCandidate2 = f.workshopComboCandidate(Set(3))
      val workshopComboCandidate3 = f.workshopComboCandidate(Set(6))
      val workshopComboCandidate4 = f.workshopComboCandidate(Set(0, 9))
      val workshopComboCandidate5 = f.workshopComboCandidate(Set(4, 13))
      val workshopComboCandidate6 = f.workshopComboCandidate(Set(6, 15))
      val workshopComboCandidate7 = f.workshopComboCandidate(Set(6, 7, 8))
      val workshopComboCandidate8: WorkshopComboCandidate = Map.empty

      hasVaryingCategories(workshopComboCandidate1) shouldEqual false
      hasVaryingCategories(workshopComboCandidate2) shouldEqual false
      hasVaryingCategories(workshopComboCandidate3) shouldEqual true
      hasVaryingCategories(workshopComboCandidate4) shouldEqual false
      hasVaryingCategories(workshopComboCandidate5) shouldEqual false
      hasVaryingCategories(workshopComboCandidate6) shouldEqual true
      hasVaryingCategories(workshopComboCandidate7) shouldEqual true
      hasVaryingCategories(workshopComboCandidate8) shouldEqual false
    }

    "generate all possible combinations of workshops from given workshops, comboSize, and matching workshops, with regards to choiceId and timeslots" in {
      val f = fixtureSymmetricWorkshops(3)

      val matchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(0) -> SelectionPriority(1), WorkshopId(1) -> SelectionPriority(1), WorkshopId(2) -> SelectionPriority(1), // WorkshopChoiceId(0)
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // WorkshopChoiceId(1)
        WorkshopId(6) -> SelectionPriority(3), WorkshopId(7) -> SelectionPriority(3), WorkshopId(8) -> SelectionPriority(3), // WorkshopChoiceId(2)
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

      val workshopCombos1 = generateWorkshopCombos(f.workshops, comboSize = 1, alwaysTrue)(matchingWorkshops)
      val workshopCombos2 = generateWorkshopCombos(f.workshops, comboSize = 2, alwaysTrue)(matchingWorkshops)
      val workshopCombos3 = generateWorkshopCombos(f.workshops, comboSize = 3, alwaysTrue)(matchingWorkshops)
      val workshopCombos4 = generateWorkshopCombos(f.workshops, comboSize = 4, alwaysTrue)(matchingWorkshops)

      workshopCombos1 should contain theSameElementsAs expectedCombos1
      workshopCombos2 should contain theSameElementsAs expectedCombos2
      workshopCombos3 should contain theSameElementsAs expectedCombos3
      workshopCombos4 should contain theSameElementsAs expectedCombos4
    }

    "generate all possible combinations of workshops from given workshops, comboSize, and matching workshops, also with regards to varying categories" in {
      val f = fixtureSymmetricWorkshops(4)

      val matchingWorkshops: MatchingWorkshops = Map(
        WorkshopId(0) -> SelectionPriority(1), WorkshopId(1) -> SelectionPriority(1), WorkshopId(2) -> SelectionPriority(1), // WorkshopChoiceId(0) health
        WorkshopId(3) -> SelectionPriority(2), WorkshopId(4) -> SelectionPriority(2), WorkshopId(5) -> SelectionPriority(2), // WorkshopChoiceId(1) relaxation
        WorkshopId(9) -> SelectionPriority(3), WorkshopId(10) -> SelectionPriority(3), WorkshopId(11) -> SelectionPriority(3), // WorkshopChoiceId(3) health again
      )
      val genCombos: Set[Set[Int]] => Set[WorkshopCombo] = f.workshopCombos(matchingWorkshops)
      val expectedCombos1 = Set.empty // because with a combo size of 1 there is no variance in categories
      val expectedCombos2 = genCombos(Set( // combos with only health or only relaxation are excluded
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

      val workshopCombos1 = generateWorkshopCombos(f.workshops, comboSize = 1, hasVaryingCategories)(matchingWorkshops)
      val workshopCombos2 = generateWorkshopCombos(f.workshops, comboSize = 2, hasVaryingCategories)(matchingWorkshops)
      val workshopCombos3 = generateWorkshopCombos(f.workshops, comboSize = 3, hasVaryingCategories)(matchingWorkshops)
      val workshopCombos4 = generateWorkshopCombos(f.workshops, comboSize = 4, hasVaryingCategories)(matchingWorkshops)

      workshopCombos1 should contain theSameElementsAs expectedCombos1
      workshopCombos2 should contain theSameElementsAs expectedCombos2
      workshopCombos3 should contain theSameElementsAs expectedCombos3
      workshopCombos4 should contain theSameElementsAs expectedCombos4
    }

    "generate all possible combinations of workshops for students from given workshops, comboSize, and the selected workshop choices of the students" in {
      val f = fixtureSymmetricWorkshops(7)

      val comboSize = 3
      val student1 = StudentId(11)
      val student2 = StudentId(12)
      val student3 = StudentId(13)
      val studentsSelectedWorkshopChoices: StudentsSelectedWorkshopChoices = Map(
        student1 -> BiMap(
          SelectionPriority(1) -> WorkshopChoiceId(0),
          SelectionPriority(2) -> WorkshopChoiceId(1),
          SelectionPriority(3) -> WorkshopChoiceId(2),
        ),
        student2 -> BiMap(
          SelectionPriority(6) -> WorkshopChoiceId(1),
          SelectionPriority(5) -> WorkshopChoiceId(3),
          SelectionPriority(4) -> WorkshopChoiceId(5),
        ),
        student3 -> BiMap( // actually an illegal choice, as all 3 workshops are of category health
          SelectionPriority(1) -> WorkshopChoiceId(0), // health
          SelectionPriority(2) -> WorkshopChoiceId(3), // health
          SelectionPriority(3) -> WorkshopChoiceId(6), // health
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
        student3 -> Set.empty, // as all 3 workshops are of category health
      )
      val expectedStudentsWorkshopCombos = expectedStudentsWsIdCombos.map { case (studentId, expectedWsIdCombos) =>
        val expectedWorkshopCombos = expectedWsIdCombos.map(expectedWsIdCombo =>
          expectedWsIdCombo
            .map(WorkshopId)
            .map { workshopId =>
              val workshop = f.workshops(workshopId)
              val category = workshop.category
              val selectionPriority = studentsSelectedWorkshopChoices(studentId).keyFor(workshop.choiceId).get
              workshopId -> PossibleWorkshop(category, selectionPriority)
            }
            .toMap)
        studentId -> expectedWorkshopCombos
      }

      val studentsWorkshopCombos = generateStudentsWorkshopCombos(f.workshops, comboSize)(studentsSelectedWorkshopChoices)

      studentsWorkshopCombos should contain theSameElementsAs expectedStudentsWorkshopCombos
    }

    "provide a method to distribute students to workshops" which {

      "yields an empty distribution if no selections were made" in {
        val f = fixtureSymmetricWorkshops(1)

        val comboSize = 3
        val studentsSelectedWorkshopChoices: StudentsSelectedWorkshopChoices = Map.empty
        val expectedDistribution = (f.workshops.view.mapValues(_ => Set.empty).toMap, Metric(0))

        distributeStudentsToWorkshops(f.workshops, comboSize)(studentsSelectedWorkshopChoices) shouldEqual expectedDistribution
      }

      "yields a valid distribution for a single student" in {
        val f = fixtureSymmetricWorkshops(4)

        val comboSize = 3
        val student1 = StudentId(1)
        val studentWorkshopSelections: StudentsSelectedWorkshopChoices = Map(
          student1 -> BiMap(
            SelectionPriority(1) -> WorkshopChoiceId(0),
            SelectionPriority(2) -> WorkshopChoiceId(1),
            SelectionPriority(3) -> WorkshopChoiceId(2),
          ),
        )
        // assumes that the algorithm orders the input so that the result is stable
        val expectedResult = (Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // WorkshopChoiceId(0)
          WorkshopId(3) -> Set.empty, WorkshopId(4) -> Set(student1), WorkshopId(5) -> Set.empty, // WorkshopChoiceId(1)
          WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty, WorkshopId(8) -> Set(student1), // WorkshopChoiceId(2)
          WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set.empty, // WorkshopChoiceId(3)
        ), Metric(6))

        distributeStudentsToWorkshops(f.workshops, comboSize)(studentWorkshopSelections) shouldEqual expectedResult
      }

    }

  }

}
