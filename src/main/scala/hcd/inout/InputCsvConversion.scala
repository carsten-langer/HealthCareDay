package hcd.inout

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import com.typesafe.scalalogging.StrictLogging
import hcd.model.TimeSlot.{FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot}
import hcd.model._
import io.cvbio.collection.mutable.bimap.BiMap

import scala.util.{Try, Using}

object InputCsvConversion extends StrictLogging {

  def readHcdWorkshopPlanning(config: CmdLineConfig): Try[(Topics, Workshops)] = {

    val csvFormat = new DefaultCSVFormat {
      override val delimiter: Char = config.wDelimiter
    }

    def toCategory(categoryStr: String): Category = categoryStr.trim match {
      case "Bewegung" => Sports
      case "Entspannung" => Relaxation
      case "Ernährung" => Nutrition
      case _ => Other
    }

    def toFlag(preassignedStr: String): Flag = preassignedStr.trim.toLowerCase match {
      case "" | "0" | "falsch" | "false" | "nein" => false
      case _ => true
    }

    def toSexes(sexStr: String): Set[Sex] = sexStr.trim match {
      case "weiblich" => Set(Female)
      case "männlich" => Set(Male)
      case _ => Set(Divers, Female, Male)
    }

    def maybeGrades(grades: String): Option[Set[Grade]] = {
      val grades1: Set[String] = grades.trim.split(',').toSet.filterNot(_.isEmpty) // deal with trailing ','
      val grades2: Set[Grade] = grades1.map(to(Grade))
      Some(grades2).filterNot(_.isEmpty)
    }

    def maybeMinSeats(seats: String): Option[Seats] = {
      val seatsString = seats.trim
      val seatsInt = (if (seatsString.isEmpty) "0" else seatsString).toInt // empty seats -> 0
      val maybeSeatsInt: Option[Int] = Option.when(seatsInt >= 0)(seatsInt) // seats < 0 -> None
      maybeSeatsInt.map(Seats)
    }

    def maybeMaxSeats(seats: String): Option[Seats] = {
      val seats1: Option[String] = Some(seats.trim).filterNot(_.isEmpty) // empty seats -> None
      val seats2: Option[Int] = seats1.map(_.toInt).filter(_ > 0) // seats <= 0 -> None
      seats2.map(Seats)
    }

    def maybeWorkshop(topicId: TopicId, timeSlot: TimeSlot, sexesStr: String, gradesStr: String, minSeatsStr: String, maxSeatsStr: String): Option[(TopicId, TimeSlot, Set[Sex], Set[Grade], Seats, Seats)] = {
      val sexes = toSexes(sexesStr)
      for {
        grades <- maybeGrades(gradesStr)
        minSeats <- maybeMinSeats(minSeatsStr)
        maxSeats <- maybeMaxSeats(maxSeatsStr)
      } yield (topicId, timeSlot, sexes, grades, minSeats, maxSeats)
    }

    Using(CSVReader.open(config.wFile)(csvFormat)) { reader =>
      val topicsWorkshops = reader
        .all()
        .slice(config.wRowsToSkip, config.wRowsToSkip + config.wNoTopics)
        .map { columns =>
          val topicId = to(TopicId)(columns(config.wColTopicId - 1))
          val topicName = columns(config.wColTopicName - 1)
          val category = toCategory(columns(config.wColCategory - 1))
          val preassigned = toFlag(columns(config.wColPreassignedTopic - 1))
          val onlyVoluntary = toFlag(columns(config.wColOnlyVoluntaryTopic - 1))
          val sexes1 = columns(config.wColSexes1 - 1)
          val grades1 = columns(config.wColGrades1 - 1)
          val minSeats1 = columns(config.wColMinSeats1 - 1)
          val maxSeats1 = columns(config.wColMaxSeats1 - 1)
          val sexes2 = columns(config.wColSexes2 - 1)
          val grades2 = columns(config.wColGrades2 - 1)
          val minSeats2 = columns(config.wColMinSeats2 - 1)
          val maxSeats2 = columns(config.wColMaxSeats2 - 1)
          val sexes3 = columns(config.wColSexes3 - 1)
          val grades3 = columns(config.wColGrades3 - 1)
          val minSeats3 = columns(config.wColMinSeats3 - 1)
          val maxSeats3 = columns(config.wColMaxSeats3 - 1)
          val ws1 = maybeWorkshop(topicId, FirstTimeSlot, sexes1, grades1, minSeats1, maxSeats1)
          val ws2 = maybeWorkshop(topicId, SecondTimeSlot, sexes2, grades2, minSeats2, maxSeats2)
          val ws3 = maybeWorkshop(topicId, ThirdTimeSlot, sexes3, grades3, minSeats3, maxSeats3)

          logger.debug(s"$topicId, $category, $preassigned, $onlyVoluntary, $topicName, s1=$sexes1, g1=$grades1, mins1=$minSeats1, maxs1=$maxSeats1, s2=$sexes2, g2=$grades2, mins2=$minSeats2, maxs2=$maxSeats2, s3=$sexes3, g3=$grades3, mins3=$minSeats3, maxs3=$maxSeats3")
          logger.trace(s"$ws1, $ws2, $ws3")

          val workshops = Seq(ws1, ws2, ws3)
            .zipWithIndex
            .collect {
              case (Some(ws@(topicId, _, _, _, _, _)), i) => (WorkshopId(topicId.id * 3 - 2 + i), ws)
            }
          ((topicId, (topicName, category, preassigned, onlyVoluntary)), workshops)
        }
      val topics = topicsWorkshops.map { case (topic, _) => topic }.toMap
      val workshops = topicsWorkshops.flatMap { case (_, workshops) => workshops }.toMap
      (topics, workshops)
    }

  }

  def readHcdStudentTopicSelection(config: CmdLineConfig): Try[StudentsNameSelectedTopics] = {

    val csvFormat = new DefaultCSVFormat {
      override val delimiter: Char = config.sDelimiter
    }

    def toSex(sexStr: String): Sex = sexStr.trim match {
      case "w" => Female
      case "m" => Male
      case _ => Divers
    }

    Using(CSVReader.open(config.sFile)(csvFormat)) { reader =>
      val unselectedTopicId = TopicId(Int.MinValue)
      val allStudentsSelectedTopics = reader
        .all()
        .slice(config.sRowsToSkip, config.sRowsToSkip + config.sNoStudents)
        .map { columns =>
          val studentId = to(StudentId)(columns(config.sColStudentId - 1))
          val studentName = columns(config.sColStudentName - 1)
          val sex = toSex(columns(config.sColSex - 1))
          val className = columns(config.sColClassName - 1)
          val grade = to(Grade)(columns(config.sColGrade - 1))
          // scan the selected topics from least to highest priority, so that in case a student has selected a topic
          // several times, it is inserted into the BiMap with the best priority
          val selectedTopics = BiMap.from(Range.inclusive(6, 1, -1)
            .map { prio =>
              val topicId = to(TopicId, unselectedTopicId)(columns(config.sColFirstSelection - 1 + prio - 1))
              val selectionPriority = SelectionPriority(prio)
              topicId -> selectionPriority
            })
          logger.debug(s"$studentId, $studentName, $grade, $selectedTopics")
          studentId -> (studentName, sex, className, grade, selectedTopics)
        }.toMap
      val studentsNameSelectedTopics = allStudentsSelectedTopics.map {
        case (studentId, (studentName, sex, className, grade, selectedTopics)) if selectedTopics.keySet.contains(unselectedTopicId) =>
          val remainingTopics = selectedTopics.filterNot { case (topicId, _) => topicId == unselectedTopicId }
          logger.debug(s"Removing non-selected topics for student $studentId, remaining topics = $remainingTopics.")
          (studentId, (studentName, sex, className, grade, remainingTopics))
        case valid => valid
      }
      studentsNameSelectedTopics
    }

  }

  private def to[A](f: Int => A)(s: String): A = f(s.trim.toInt)

  private def to[A](f: Int => A, default: A)(s: String): A = Try(f(s.trim.toInt)).getOrElse(default)

}
