package hcd.algorithms

import com.typesafe.scalalogging.StrictLogging
import hcd.model.TimeSlot.{FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot}
import hcd.model._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FixtureWorkshopsSpec
  extends AnyWordSpec
    with Matchers
    with StrictLogging {

  "FixtureFullDataModel" should {

    "build test data correctly and optionally print it" in {
      val f = new FixtureFullDataModel {}

      f.topics(TopicId(0)) shouldEqual Nutrition
      f.topics(TopicId(1)) shouldEqual Relaxation
      f.topics(TopicId(2)) shouldEqual Sports
      f.topics(TopicId(3)) shouldEqual Other
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
