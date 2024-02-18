# Health Care Day

Software support for a health care day including an algorithm to distribute students to workshops taking their
preferences into account. The distribution is a typical combinatorial optimization problem.

In the beginning the focus is on solving the combinatorial optimization problem. Later on, more general support can be
added like supporting the administrative handling of students and workshop.

The _Health Care Day_ is a special day at our school on which all students will not attend normal classes, but instead
attend special workshops around health and well-being.

On the day itself there are three __timeslots__, _first_, _second_, _third_. Each student shall attend one workshop in
each timeslot.

A __workshop__ is a concrete activity, at one timeslot, at one place, with one trainer etc. It will be on a certain
__topic__, like _meditation_, _soccer_, _diving_, _cooking_, ... The workshop topic also belongs to a more coarse
grained topic __category__ of _nutrition_, _relaxation_, and _sports_.

Upfront, the students can choose a certain number of topics from the list of all workshop topics.
They are allowed to give a __selection priority__ to their workshop topic choices.

Most workshop topics will be offered in all the three timeslots. That is, behind a workshop topic there are typically
three actual workshops to which a student could be distributed. But not all workshop topics are offered on all three
timeslots. Also, the workshops may have a different number of __available seats__.

The challenge is to find a distribution of students to workshops so that:

* All students attend in each timeslot one workshop.
* No workshop is booked beyond its capacity of available seats.
* The students are distributed to workshops which ideally are high on their workshop topic selection priority.
* Further limitations may apply, like e.g. no student shall be distributed to three workshops of the same category.
* If several distributions are possible, a _better_ distribution should be preferred according to a customizable metric.

While this combinatorial challenge is probably completely solvable for small numbers, our challenge is to find a
solution for about these numbers:

* Ca. 600 students.
* Ca. 50 workshop topics to choose from.
* Therefore, ca. 150 actual workshops, the majority of workshop topics are backed by three workshops, one in each
  timeslot, but not all of them.
* In average, min. 12 seats per workshop to provide one seat per student per workshop (50 * 12 = 600), but this largely
  depends on the workshop and may differ wildly from e.g. as low as 5 to as many as 50 seats.
* Students are allowed to pick 6 out of the 50 workshop topics and give them their selection priority from 1 (most
  wanted) to 6 (least wanted).

As further limitations in distributing the students to the actual workshops we currently plan:

* No student shall be distributed to three workshops from only category _nutrition_, nor to three workshops from only
  category _relaxation_. It is OK but however not encouraged (lower metric) to distribute a student to three workshops
  from only category _sports_.
* Each student should at least get one workshop assigned from the list of chosen workshop topics with selection
  preference of 1, 2, or 3. This is a minimum requirement. In general, the better the actual workshops the student is
  assigned to fit to the student's selection priority, the better the distribution (metric).

It is expected that the combinatorial challenge is not completely solvable due to the sheer combinatorial complexity and
that some form of approximation is needed to find a suboptimal yet sufficiently good distribution.

## ToDos

* The current approach still tries a full combinatorial solution, just to see how far we can come. No approximation
  algorithm was yet tried.
