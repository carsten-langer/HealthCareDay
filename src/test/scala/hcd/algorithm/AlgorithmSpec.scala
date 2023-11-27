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

      def workshopsForIds(ids: Set[Int]): Workshops = ids
        .map(WorkshopId)
        .map(workshopId => (workshopId, workshops(workshopId))) // @throws[NoSuchElementException] to protect against wrong test assumptions
        .toMap

      // create selected workshops for Map(selectionPriority -> Set(workshopId))
      def selectedWorkshopsFor(selIds: Map[Int, Set[Int]]): SelectedWorkshops = selIds.flatMap {
        case (selectionPriorityInt, workshopIdsInt) => workshopIdsInt
          .map(WorkshopId)
          .map { workshopId =>
            val Workshop(category, choiceId, timeSlot, _) = workshops(workshopId) // @throws[NoSuchElementException] to protect against wrong test assumptions
            (workshopId, SelectedWorkshop(category, choiceId, timeSlot, SelectionPriority(selectionPriorityInt)))
          }
      }

      // create possible workshops for Set(selectionPriority -> workshopId)
      def possibleWorkshopsFor(selIds: Set[(Int, Int)]): PossibleWorkshops = selIds.map {
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
      @unused // may be unused, depending on whether the model is printed out our not
      lazy val workshopSelections: StudentWorkshopSelections = f.studentIds.map(
        (_, BiMap.from(f.selectionPriorities.zip(Random.shuffle(f.workshopChoiceIds.toSeq))))
      ).toMap

      // print workshop selections ordered by student id
      //workshopSelections.toSeq.sortBy(_._1.id).foreach(println)

      // print studentsSelectedWorkshopsFromStudentWorkshopSelections for full model
      //import scala.collection.SortedMap
      //studentsSelectedWorkshopsFromStudentWorkshopSelections(f.workshops)(workshopSelections)
      //  .toSeq.sortBy(_._1.id).foreach(t => println(t._1, SortedMap.from(t._2)(Ordering.by(_.id))))
    }

    "select Workshops from WorkshopChoiceId" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(2)
      val fut: WorkshopChoiceId => Workshops = workshopsFromWorkshopChoiceId(f.workshops)

      val workshopChoiceId1 = WorkshopChoiceId(0)
      val workshopChoiceId2 = WorkshopChoiceId(1)
      val expectedWorkshops1 = f.workshopsForIds(Set(0, 1, 2))
      val expectedWorkshops2 = f.workshopsForIds(Set(3, 4, 5))

      fut(workshopChoiceId1) should contain theSameElementsAs expectedWorkshops1
      fut(workshopChoiceId2) should contain theSameElementsAs expectedWorkshops2
    }

    "select SelectedWorkshops from WorkshopSelection" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(4)
      val fut: WorkshopSelection => SelectedWorkshops = selectedWorkshopsFromWorkshopSelection(f.workshops)

      val workshopSelection1 = BiMap(
        SelectionPriority(1) -> WorkshopChoiceId(0),
        SelectionPriority(2) -> WorkshopChoiceId(1)
      )
      val workshopSelection2 = BiMap(
        SelectionPriority(5) -> WorkshopChoiceId(3),
        SelectionPriority(6) -> WorkshopChoiceId(2)
      )
      val expectedWorkshops1 = f.selectedWorkshopsFor(Map(1 -> Set(0, 1, 2), 2 -> Set(3, 4, 5)))
      val expectedWorkshops2 = f.selectedWorkshopsFor(Map(5 -> Set(9, 10, 11), 6 -> Set(6, 7, 8)))

      fut(workshopSelection1) should contain theSameElementsAs expectedWorkshops1
      fut(workshopSelection2) should contain theSameElementsAs expectedWorkshops2
    }

    "select SelectedWorkshops per student from StudentWorkshopSelections" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(19)
      val fut: StudentWorkshopSelections => Map[StudentId, SelectedWorkshops] = studentsSelectedWorkshopsFromStudentWorkshopSelections(f.workshops)

      val student1 = StudentId(5)
      val student2 = StudentId(42)
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
        )
      )
      val expectedWorkshops1 = f.selectedWorkshopsFor(Map(7 -> Set(42, 43, 44), 9 -> Set(54, 55, 56), 5 -> Set(30, 31, 32)))
      val expectedWorkshops2 = f.selectedWorkshopsFor(Map(6 -> Set(9, 10, 11), 2 -> Set(3, 4, 5), 4 -> Set(6, 7, 8)))

      fut(studentWorkshopSelections) should contain theSameElementsAs Map(
        student1 -> expectedWorkshops1,
        student2 -> expectedWorkshops2
      )
    }

    "filter SelectedWorkshops via haveDistinctChoiceIds" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(2)

      val selectedWorkshops1 = f.selectedWorkshopsFor(Map(1 -> Set(0, 1)))
      val selectedWorkshops2 = f.selectedWorkshopsFor(Map(1 -> Set(0), 2 -> Set(5)))
      val selectedWorkshops3: SelectedWorkshops = Map.empty

      haveDistinctChoiceIds(selectedWorkshops1) shouldEqual false
      haveDistinctChoiceIds(selectedWorkshops2) shouldEqual true
      haveDistinctChoiceIds(selectedWorkshops3) shouldEqual false
    }

    "filter SelectedWorkshops via haveDistinctTimeslots" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(3)

      val selectedWorkshops1 = f.selectedWorkshopsFor(Map(1 -> Set(0, 1, 2), 2 -> Set(3, 4, 5)))
      val selectedWorkshops2 = f.selectedWorkshopsFor(Map(1 -> Set(0, 1), 2 -> Set(5), 3 -> Set(7)))
      val selectedWorkshops3 = f.selectedWorkshopsFor(Map(1 -> Set(0), 2 -> Set(3)))
      val selectedWorkshops4 = f.selectedWorkshopsFor(Map(1 -> Set(0), 2 -> Set(8), 3 -> Set(4)))
      val selectedWorkshops5: SelectedWorkshops = Map.empty

      haveDistinctTimeslots(selectedWorkshops1) shouldEqual false
      haveDistinctTimeslots(selectedWorkshops2) shouldEqual false
      haveDistinctTimeslots(selectedWorkshops3) shouldEqual false
      haveDistinctTimeslots(selectedWorkshops4) shouldEqual true
      haveDistinctTimeslots(selectedWorkshops5) shouldEqual false
    }

    "select from a selection of Workshops all possible combinations of N workshops with regards to choiceId and timeslots" in {
      val f = fixtureSymmetricWorkshopsNoSeatsLimit(3)

      val selectedWorkshops = f.selectedWorkshopsFor(Map(1 -> Set(0, 1, 2), 2 -> Set(3, 4, 5), 3 -> Set(6, 7, 8)))
      val expectedCombinations1 = Seq(
        f.possibleWorkshopsFor(Set(1 -> 0)),
        f.possibleWorkshopsFor(Set(1 -> 1)),
        f.possibleWorkshopsFor(Set(1 -> 2)),
        f.possibleWorkshopsFor(Set(2 -> 3)),
        f.possibleWorkshopsFor(Set(2 -> 4)),
        f.possibleWorkshopsFor(Set(2 -> 5)),
        f.possibleWorkshopsFor(Set(3 -> 6)),
        f.possibleWorkshopsFor(Set(3 -> 7)),
        f.possibleWorkshopsFor(Set(3 -> 8)),
      )
      val expectedCombinations2 = Seq(
        f.possibleWorkshopsFor(Set(1 -> 0, 2 -> 4)),
        f.possibleWorkshopsFor(Set(1 -> 0, 2 -> 5)),
        f.possibleWorkshopsFor(Set(1 -> 0, 3 -> 7)),
        f.possibleWorkshopsFor(Set(1 -> 0, 3 -> 8)),
        f.possibleWorkshopsFor(Set(1 -> 1, 2 -> 3)),
        f.possibleWorkshopsFor(Set(1 -> 1, 2 -> 5)),
        f.possibleWorkshopsFor(Set(1 -> 1, 3 -> 6)),
        f.possibleWorkshopsFor(Set(1 -> 1, 3 -> 8)),
        f.possibleWorkshopsFor(Set(1 -> 2, 2 -> 3)),
        f.possibleWorkshopsFor(Set(1 -> 2, 2 -> 4)),
        f.possibleWorkshopsFor(Set(1 -> 2, 3 -> 6)),
        f.possibleWorkshopsFor(Set(1 -> 2, 3 -> 7)),
        f.possibleWorkshopsFor(Set(2 -> 3, 3 -> 7)),
        f.possibleWorkshopsFor(Set(2 -> 3, 3 -> 8)),
        f.possibleWorkshopsFor(Set(2 -> 4, 3 -> 6)),
        f.possibleWorkshopsFor(Set(2 -> 4, 3 -> 8)),
        f.possibleWorkshopsFor(Set(2 -> 5, 3 -> 6)),
        f.possibleWorkshopsFor(Set(2 -> 5, 3 -> 7)),
      )
      val expectedCombinations3 = Seq(
        f.possibleWorkshopsFor(Set(1 -> 0, 2 -> 4, 3 -> 8)),
        f.possibleWorkshopsFor(Set(1 -> 0, 2 -> 5, 3 -> 7)),
        f.possibleWorkshopsFor(Set(1 -> 1, 2 -> 3, 3 -> 8)),
        f.possibleWorkshopsFor(Set(1 -> 1, 2 -> 5, 3 -> 6)),
        f.possibleWorkshopsFor(Set(1 -> 2, 2 -> 3, 3 -> 7)),
        f.possibleWorkshopsFor(Set(1 -> 2, 2 -> 4, 3 -> 6)),
      )
      val expectedCombinations4 = Seq.empty // because a combo of 4 will always overlap on timeslots

      possibleWorkshopCombinations(1)(selectedWorkshops) should contain theSameElementsAs expectedCombinations1
      possibleWorkshopCombinations(2)(selectedWorkshops) should contain theSameElementsAs expectedCombinations2
      possibleWorkshopCombinations(3)(selectedWorkshops) should contain theSameElementsAs expectedCombinations3
      possibleWorkshopCombinations(4)(selectedWorkshops) should contain theSameElementsAs expectedCombinations4
    }

  }

}