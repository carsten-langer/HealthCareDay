package hcd

import hcd.model._

package object algorithms {

  def fixtureSymmetricWorkshopsFor(noTopics: Int, _noSeats: Int): FixtureWorkshops = new FixtureWorkshops {
    // Inputs for model size
    private val timeSlots = Seq(FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot)
    private val categories = Seq(Nutrition, Relaxation, Sports, Other)
    private val noWorkshops = noTopics * timeSlots.size // all workshop topics are available on all timeslots

    // Generate all IDs
    private val topicIds = Range(0, noTopics).map(TopicId)
    private val workshopIds = Range(0, noWorkshops).map(WorkshopId)

    // Generate symmetric workshops:
    // workshop categories are equally distributed among topics
    // each workshop topic exists in all timeslot
    // no limits of workshop seats
    // categories alter n,n,n, r,r,r, s,s,s, o,o,o, n,n,n, ...
    // topicIds alter 0,0,0, 1,1,1, 2,2,2, 3,3,3, 4,4,4, ...
    // timeslots alter f,s,t, f,s,t, f,s,t, f,s,t, f,s,t, ...
    override val topics: Topics = topicIds.map(topicId => topicId -> categories(topicId.id % categories.size)).toMap
    override val noSeats: Int = _noSeats
    private val seats = Seats(noSeats)
    override val workshops: Workshops = workshopIds.map(workshopId =>
      workshopId -> (
        TopicId(workshopId.id / timeSlots.size),
        timeSlots(workshopId.id % timeSlots.size),
        grades,
        seats,
      )
    ).toMap
  }

  def fixtureSymmetricWorkshopsFor(noTopics: Int): FixtureWorkshops =
    fixtureSymmetricWorkshopsFor(noTopics, _noSeats = 12)

}
