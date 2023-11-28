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

    trait Fixture {
      def workshops: Workshops

      // create selected workshops for Map(selectionPriority -> Set(workshopId))
      def selectedWorkshopsFrom(selIds: Map[Int, Set[Int]]): SelectedWorkshops = selIds.flatMap {
        case (selectionPriorityInt, workshopIdsInt) => workshopIdsInt
          .map(WorkshopId)
          .map { workshopId =>
            val Workshop(category, choiceId, timeSlot, _) = workshops(workshopId) // @throws[NoSuchElementException] to protect against wrong test assumptions
            (workshopId, SelectedWorkshop(category, choiceId, timeSlot, SelectionPriority(selectionPriorityInt)))
          }
      }

      // create possible workshops for Set(selectionPriority -> workshopId)
      def possibleWorkshopsFrom(selIds: Set[(Int, Int)]): PossibleWorkshops = selIds.map {
        case (selectionPriorityInt, workshopIdInt) =>
          val workshopId = WorkshopId(workshopIdInt)
          val Workshop(category, _, _, _) = workshops(workshopId) // @throws[NoSuchElementException] to protect against wrong test assumptions
          (workshopId, PossibleWorkshop(category, SelectionPriority(selectionPriorityInt)))
      }.toMap
    }

    def fixtureSymmetricWorkshopsNoSeatsLimit(noWorkshopChoices: Int): Fixture = new Fixture {
      // Inputs for model size
      private val timeSlots = Seq(FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)
      private val categories = Seq(Health, Relaxation, Sports)
      private val noWorkshops = timeSlots.size * noWorkshopChoices // all workshops are available on all timeslots

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
          seats = Int.MaxValue
        ))
      ).toMap
    }

    def fixtureFullDataModel = new {
      // Inputs for model size
      private val noWorkshopChoices = 50
      private val noStudents = 1000
      private val noSelectionsPerStudent = 6

      // Generate all IDs
      lazy val workshopChoiceIds: Set[WorkshopChoiceId] = Range(0, noWorkshopChoices).toSet.map(WorkshopChoiceId)
      lazy val studentIds: Set[StudentId] = Range(0, noStudents).toSet.map(StudentId)
      lazy val selectionPriorities: Set[SelectionPriority] = Range.inclusive(1, noSelectionsPerStudent).toSet.map(SelectionPriority)

      // Generate symmetric workshops with no seats limit
      val workshops: Workshops = fixtureSymmetricWorkshopsNoSeatsLimit(noWorkshopChoices).workshops
    }

    "build test data correctly and optionally print it" in {
      val f = fixtureFullDataModel

      f.workshops(WorkshopId(0)) shouldEqual Workshop(Health, WorkshopChoiceId(0), FirstTimeSlot, Int.MaxValue)
      f.workshops(WorkshopId(4)) shouldEqual Workshop(Relaxation, WorkshopChoiceId(1), SecondTimeSlot, Int.MaxValue)
      f.workshops(WorkshopId(8)) shouldEqual Workshop(Sports, WorkshopChoiceId(2), ThirdTimeSlot, Int.MaxValue)

      // print workshops ordered by id
      //f.workshops.toSeq.sortBy(_._1.id).foreach(println)

      // generate random workshop selections
      Random.setSeed(0L) // fix randomness during development
      lazy val studentWorkshopSelections: StudentWorkshopSelections = f.studentIds.map(
        (_, BiMap.from(f.selectionPriorities.zip(Random.shuffle(f.workshopChoiceIds.toSeq))))
      ).toMap

      // print workshop selections ordered by student id
      //studentWorkshopSelections.toSeq.sortBy(_._1.id).foreach(println)

      // print studentsSelectedWorkshopsFromStudentWorkshopSelections for full model
      @unused // may be unused, depending on whether the model is printed out our not
      lazy val studentsSelectedWorkshops = studentsSelectedWorkshopsFromStudentWorkshopSelections(f.workshops)(studentWorkshopSelections)
      //studentsSelectedWorkshops.toSeq.sortBy(_._1.id).foreach(t => println(t._1, collection.SortedMap.from(t._2)(Ordering.by(_.id))))

      // print studentsPossibleWorkshops for full model
      lazy val studentsPossibleWorkshops = possibleWorkshopCombinations(f.workshops, 3)(studentWorkshopSelections)
      //println(studentsPossibleWorkshops)

      // print distributeStudentsToWorkshops for full model
      lazy val (workshopAssignments, metric) = distributeStudentsToWorkshops(f.workshops)(studentsPossibleWorkshops)
      if (System.getProperty("DistributeStudentsToWorkshops", "false").toBooleanOption.getOrElse(false))
        println(workshopAssignments, metric)

    }

    "select SelectedWorkshops from WorkshopSelection" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(4)
      val fut: WorkshopSelection => SelectedWorkshops = selectedWorkshopsFromWorkshopSelection(f.workshops)

      val workshopSelection1 = BiMap(
        SelectionPriority(1) -> WorkshopChoiceId(0),
        SelectionPriority(2) -> WorkshopChoiceId(1),
      )
      val workshopSelection2 = BiMap(
        SelectionPriority(5) -> WorkshopChoiceId(3),
        SelectionPriority(6) -> WorkshopChoiceId(2),
      )
      val workshopSelection3 = BiMap(
        SelectionPriority(5) -> WorkshopChoiceId(4), // non-existing workshop choice
      )
      val expectedWorkshops1 = f.selectedWorkshopsFrom(Map(1 -> Set(0, 1, 2), 2 -> Set(3, 4, 5)))
      val expectedWorkshops2 = f.selectedWorkshopsFrom(Map(5 -> Set(9, 10, 11), 6 -> Set(6, 7, 8)))

      fut(workshopSelection1) should contain theSameElementsAs expectedWorkshops1
      fut(workshopSelection2) should contain theSameElementsAs expectedWorkshops2
      fut(workshopSelection3) shouldBe empty
    }

    "select SelectedWorkshops per student from StudentWorkshopSelections" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(19)
      val fut: StudentWorkshopSelections => Map[StudentId, SelectedWorkshops] = studentsSelectedWorkshopsFromStudentWorkshopSelections(f.workshops)

      val student1 = StudentId(5)
      val student2 = StudentId(42)
      val student3 = StudentId(-1)
      val studentWorkshopSelections = Map(
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
      val expectedWorkshops1 = f.selectedWorkshopsFrom(Map(7 -> Set(42, 43, 44), 9 -> Set(54, 55, 56), 5 -> Set(30, 31, 32)))
      val expectedWorkshops2 = f.selectedWorkshopsFrom(Map(6 -> Set(9, 10, 11), 2 -> Set(3, 4, 5), 4 -> Set(6, 7, 8)))
      val expectedWorkshops3: SelectedWorkshops = Map.empty
      val expectedStudentSelectedWorkshops = Map(
        student1 -> expectedWorkshops1,
        student2 -> expectedWorkshops2,
        student3 -> expectedWorkshops3,
      )

      fut(studentWorkshopSelections) should contain theSameElementsAs expectedStudentSelectedWorkshops
    }

    "filter SelectedWorkshops via haveDistinctChoiceIds" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(2)

      val selectedWorkshops1 = f.selectedWorkshopsFrom(Map(1 -> Set(0, 1)))
      val selectedWorkshops2 = f.selectedWorkshopsFrom(Map(2 -> Set(0, 2, 3)))
      val selectedWorkshops3 = f.selectedWorkshopsFrom(Map(3 -> Set(0, 3))) // artificial combination, with the symmetric fixture such choice could not have happened
      val selectedWorkshops4 = f.selectedWorkshopsFrom(Map(4 -> Set(0), 5 -> Set(5)))
      val selectedWorkshops5: SelectedWorkshops = Map.empty

      haveDistinctChoiceIds(selectedWorkshops1) shouldEqual false
      haveDistinctChoiceIds(selectedWorkshops2) shouldEqual false
      haveDistinctChoiceIds(selectedWorkshops3) shouldEqual true
      haveDistinctChoiceIds(selectedWorkshops4) shouldEqual true
      haveDistinctChoiceIds(selectedWorkshops5) shouldEqual false
    }

    "filter SelectedWorkshops via haveDistinctTimeslots" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(3)

      val selectedWorkshops1 = f.selectedWorkshopsFrom(Map(1 -> Set(0, 1, 2), 2 -> Set(3, 4, 5)))
      val selectedWorkshops2 = f.selectedWorkshopsFrom(Map(3 -> Set(0, 1), 4 -> Set(5), 5 -> Set(7)))
      val selectedWorkshops3 = f.selectedWorkshopsFrom(Map(6 -> Set(0), 7 -> Set(5)))
      val selectedWorkshops4 = f.selectedWorkshopsFrom(Map(8 -> Set(1), 9 -> Set(5)))
      val selectedWorkshops5 = f.selectedWorkshopsFrom(Map(10 -> Set(2), 11 -> Set(5)))
      val selectedWorkshops6 = f.selectedWorkshopsFrom(Map(12 -> Set(0), 13 -> Set(8), 14 -> Set(4)))
      val selectedWorkshops7: SelectedWorkshops = Map.empty

      haveDistinctTimeslots(selectedWorkshops1) shouldEqual false
      haveDistinctTimeslots(selectedWorkshops2) shouldEqual false
      haveDistinctTimeslots(selectedWorkshops3) shouldEqual true
      haveDistinctTimeslots(selectedWorkshops4) shouldEqual true
      haveDistinctTimeslots(selectedWorkshops5) shouldEqual false
      haveDistinctTimeslots(selectedWorkshops6) shouldEqual true
      haveDistinctTimeslots(selectedWorkshops7) shouldEqual false
    }

    "select from a selection of Workshops all possible combinations of N workshops with regards to choiceId and timeslots" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(3)

      val selectedWorkshops = f.selectedWorkshopsFrom(Map(1 -> Set(0, 1, 2), 2 -> Set(3, 4, 5), 3 -> Set(6, 7, 8)))
      val expectedCombinations1 = Set(
        f.possibleWorkshopsFrom(Set(1 -> 0)),
        f.possibleWorkshopsFrom(Set(1 -> 1)),
        f.possibleWorkshopsFrom(Set(1 -> 2)),
        f.possibleWorkshopsFrom(Set(2 -> 3)),
        f.possibleWorkshopsFrom(Set(2 -> 4)),
        f.possibleWorkshopsFrom(Set(2 -> 5)),
        f.possibleWorkshopsFrom(Set(3 -> 6)),
        f.possibleWorkshopsFrom(Set(3 -> 7)),
        f.possibleWorkshopsFrom(Set(3 -> 8)),
      )
      val expectedCombinations2 = Set(
        f.possibleWorkshopsFrom(Set(1 -> 0, 2 -> 4)),
        f.possibleWorkshopsFrom(Set(1 -> 0, 2 -> 5)),
        f.possibleWorkshopsFrom(Set(1 -> 0, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(1 -> 0, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 2 -> 3)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 2 -> 5)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 3 -> 6)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 2 -> 3)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 2 -> 4)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 3 -> 6)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(2 -> 3, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(2 -> 3, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(2 -> 4, 3 -> 6)),
        f.possibleWorkshopsFrom(Set(2 -> 4, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(2 -> 5, 3 -> 6)),
        f.possibleWorkshopsFrom(Set(2 -> 5, 3 -> 7)),
      )
      val expectedCombinations3 = Set(
        f.possibleWorkshopsFrom(Set(1 -> 0, 2 -> 4, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(1 -> 0, 2 -> 5, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 2 -> 3, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 2 -> 5, 3 -> 6)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 2 -> 3, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 2 -> 4, 3 -> 6)),
      )
      val expectedCombinations4 = Seq.empty // because a combo of 4 will always overlap on timeslots

      possibleWorkshopCombinations(1)(selectedWorkshops) should contain theSameElementsAs expectedCombinations1
      possibleWorkshopCombinations(2)(selectedWorkshops) should contain theSameElementsAs expectedCombinations2
      possibleWorkshopCombinations(3)(selectedWorkshops) should contain theSameElementsAs expectedCombinations3
      possibleWorkshopCombinations(4)(selectedWorkshops) should contain theSameElementsAs expectedCombinations4
    }

    "generate all possible combinations of workshops for students from given Workshops, the number of to-be-taken workshops N and the StudentWorkshopSelections" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(6)

      val n = 3
      val student1 = StudentId(11)
      val student2 = StudentId(12)
      val studentWorkshopSelections = Map(
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
      )

      val expectedCombinations1 = Set(
        f.possibleWorkshopsFrom(Set(1 -> 0, 2 -> 4, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(1 -> 0, 2 -> 5, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 2 -> 3, 3 -> 8)),
        f.possibleWorkshopsFrom(Set(1 -> 1, 2 -> 5, 3 -> 6)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 2 -> 3, 3 -> 7)),
        f.possibleWorkshopsFrom(Set(1 -> 2, 2 -> 4, 3 -> 6)),
      )
      val expectedCombinations2 = Set(
        f.possibleWorkshopsFrom(Set(6 -> 3, 5 -> 10, 4 -> 17)),
        f.possibleWorkshopsFrom(Set(6 -> 3, 5 -> 11, 4 -> 16)),
        f.possibleWorkshopsFrom(Set(6 -> 4, 5 -> 9, 4 -> 17)),
        f.possibleWorkshopsFrom(Set(6 -> 4, 5 -> 11, 4 -> 15)),
        f.possibleWorkshopsFrom(Set(6 -> 5, 5 -> 9, 4 -> 16)),
        f.possibleWorkshopsFrom(Set(6 -> 5, 5 -> 10, 4 -> 15)),
      )
      val expectedStudentSelectedWorkshops = Map(
        student1 -> expectedCombinations1,
        student2 -> expectedCombinations2,
      )

      possibleWorkshopCombinations(f.workshops, n)(studentWorkshopSelections) should contain theSameElementsAs expectedStudentSelectedWorkshops
    }

    "provide a method to distribute students to workshops" which {

      "yields an empty distribution if no selections were made" in {
        val f = fixtureSymmetricWorkshopsNoSeatsLimit(1)

        val n = 3
        val studentWorkshopSelections: StudentWorkshopSelections = Map.empty
        val studentPossibleWorkshops = possibleWorkshopCombinations(f.workshops, n)(studentWorkshopSelections)
        val expectedDistribution = (f.workshops.view.mapValues(_ => Set.empty).toMap, 0)

        distributeStudentsToWorkshops(f.workshops)(studentPossibleWorkshops) shouldEqual expectedDistribution
      }

      "yields a valid distribution for a single student" in {
        val f = fixtureSymmetricWorkshopsNoSeatsLimit(4)

        val n = 3
        val student1 = StudentId(1)
        val studentWorkshopSelections: StudentWorkshopSelections = Map(
          student1 -> BiMap(
            SelectionPriority(1) -> WorkshopChoiceId(0),
            SelectionPriority(2) -> WorkshopChoiceId(1),
            SelectionPriority(3) -> WorkshopChoiceId(2),
          ),
        )
        val studentPossibleWorkshops = possibleWorkshopCombinations(f.workshops, n)(studentWorkshopSelections)
        // assumes that the algorithm orders the input so that the result is stable
        val expectedResult = (Map(
          WorkshopId(0) -> Set(student1),
          WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, WorkshopId(3) -> Set.empty,
          WorkshopId(4) -> Set(student1),
          WorkshopId(5) -> Set.empty, WorkshopId(6) -> Set.empty, WorkshopId(7) -> Set.empty,
          WorkshopId(8) -> Set(student1),
          WorkshopId(9) -> Set.empty, WorkshopId(10) -> Set.empty, WorkshopId(11) -> Set.empty,
        ), 6)

        distributeStudentsToWorkshops(f.workshops)(studentPossibleWorkshops) shouldEqual expectedResult
      }

    }

  }

}