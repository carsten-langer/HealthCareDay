package hcd.algorithms

import hcd.model.TimeSlot.{FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot}
import hcd.model._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FixtureWorkshopsSpec
  extends AnyWordSpec
    with Matchers
    with Inside
    //with StrictLogging
{

  "FixtureFullDataModel" should {

    "build test data correctly and optionally print it" in {
      val f = new FixtureFullDataModel {}

      inside(f.topics(TopicId(0))) { case (_, category, _, _) => category shouldEqual Nutrition }
      inside(f.topics(TopicId(1))) { case (_, category, _, _) => category shouldEqual Relaxation }
      inside(f.topics(TopicId(2))) { case (_, category, _, _) => category shouldEqual Sports }
      inside(f.topics(TopicId(3))) { case (_, category, _, _) => category shouldEqual Other }
      f.workshops(WorkshopId(0)) shouldEqual(TopicId(0), FirstTimeSlot, f.grades, Seats(f.noSeats))
      f.workshops(WorkshopId(4)) shouldEqual(TopicId(1), SecondTimeSlot, f.grades, Seats(f.noSeats))
      f.workshops(WorkshopId(8)) shouldEqual(TopicId(2), ThirdTimeSlot, f.grades, Seats(f.noSeats))

      // print workshops ordered by id
      //f.workshops.toSeq.sortBy(_._1.id).foreach(w => logger.info(w.toString))

      // print students' selected workshop topics ordered by student id
      //f.studentsSelectedTopics.toSeq.sortBy(_._1.id).foreach(sst => logger.info(sst.toString))
    }

  }

}
