package hcd.inout

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import com.typesafe.scalalogging.StrictLogging
import hcd.model.TimeSlot.{FirstTimeSlot, SecondTimeSlot, ThirdTimeSlot}
import hcd.model._
import io.cvbio.collection.mutable.bimap.BiMap

import scala.util.{Try, Using}

object InputCsvConversion extends StrictLogging {

  def readHcdWorkshopPlanning(config: InputConfig): Try[(TopicsWithName, Workshops)] = {

    val csvFormat = new DefaultCSVFormat {
      override val delimiter: Char = config.wDelimiter
    }

    def toCategory(categoryStr: String): Category = categoryStr.trim match {
      case "Bewegung" => Sports
      case "Entspannung" => Relaxation
      case "Ernährung" => Nutrition
      case _ => Other
    }

    def maybeGrades(grades: String): Option[Set[Grade]] = {
      val grades1: Set[String] = grades.trim.split(',').toSet.filterNot(_.isEmpty) // deal with trailing ','
      val grades2: Set[Grade] = grades1.map(to(Grade))
      Some(grades2).filterNot(_.isEmpty)
    }

    def maybeSeats(seats: String): Option[Seats] = {
      val seats1: Option[String] = Some(seats.trim).filterNot(_.isEmpty) // empty seats -> None
      val seats2: Option[Int] = seats1.map(_.toInt).filter(_ > 0) // seats <= 0 -> None
      seats2.map(Seats)
    }

    def maybeWorkshop(topicId: TopicId, timeSlot: TimeSlot, gradesStr: String, seatsStr: String): Option[(TopicId, TimeSlot, Set[Grade], Seats)] =
      for {
        grades <- maybeGrades(gradesStr)
        seats <- maybeSeats(seatsStr)
      } yield (topicId, timeSlot, grades, seats)

    Using(CSVReader.open(config.wFile)(csvFormat)) { reader =>
      val allTopicsWorkshops = reader
        .all()
        .slice(config.wRowsToSkip, config.wRowsToSkip + config.wNoTopics)
        .map { columns =>
          val topicId = to(TopicId)(columns(config.wColTopicId - 1))
          val topicName = columns(config.wColTopicName - 1)
          val category = toCategory(columns(config.wColCategory - 1))
          val grades1 = columns(config.wColGrades1 - 1)
          val seats1 = columns(config.wColSeats1 - 1)
          val grades2 = columns(config.wColGrades2 - 1)
          val seats2 = columns(config.wColSeats2 - 1)
          val grades3 = columns(config.wColGrades3 - 1)
          val seats3 = columns(config.wColSeats3 - 1)
          val ws1 = maybeWorkshop(topicId, FirstTimeSlot, grades1, seats1)
          val ws2 = maybeWorkshop(topicId, SecondTimeSlot, grades2, seats2)
          val ws3 = maybeWorkshop(topicId, ThirdTimeSlot, grades3, seats3)

          logger.trace(s"$topicId, $category, $topicName, g1=$grades1, s1=$seats1, g2=$grades2, s2=$seats2, g3=$grades3, s3=$seats3")
          logger.trace(s"$ws1, $ws2, $ws3")

          val workshops = Seq(ws1, ws2, ws3)
            .zipWithIndex
            .collect {
              case (Some(ws@(topicId, _, _, _)), i) => (WorkshopId(topicId.id * 3 - 2 + i), ws)
            }
          ((topicId, (topicName, category)), workshops)
        }
      val topicsWorkshops = allTopicsWorkshops.filter {
        case ((topicId, (topicName, _)), _) if excludedTopics(config).contains(topicId) =>
          logger.debug(s"Excluding full-day topic $topicId ($topicName) from the distribution.")
          false
        case _ => true
      }
      val topics = topicsWorkshops.map { case (topic, _) => topic }.toMap
      val workshops = topicsWorkshops.flatMap { case (_, workshops) => workshops }.toMap
      (topics, workshops)
    }

  }

  def readHcdStudentTopicSelection(config: InputConfig): Try[StudentsSelectedTopicsWithName] = {

    val csvFormat = new DefaultCSVFormat {
      override val delimiter: Char = config.sDelimiter
    }

    Using(CSVReader.open(config.sFile)(csvFormat)) { reader =>
      val allStudentsSelectedTopics = reader
        .all()
        .slice(config.sRowsToSkip, config.sRowsToSkip + config.sNoStudents)
        .map { columns =>
          val studentId = to(StudentId)(columns(config.sColStudentId - 1))
          val studentName = columns(config.sColStudentName - 1)
          val grade = to(Grade)(columns(config.sColGrade - 1))
          val selectedTopics = BiMap.from(Range.inclusive(1, 6)
            .map { prio =>
              val topicId = to(TopicId)(columns(config.sColFirstSelection - 1 + prio - 1))
              val selectionPriority = SelectionPriority(prio)
              topicId -> selectionPriority
            })
          logger.trace(s"$studentId, $studentName, $grade, $selectedTopics")
          studentId -> (studentName, grade, selectedTopics)
        }.toMap
      val unselectedTopicId = TopicId(0)
      val studentsSelectedTopicsWithName = allStudentsSelectedTopics.flatMap {
        case (studentId, (studentName, _, selectedTopics)) if selectedTopics.keySet.intersect(excludedTopics(config)).nonEmpty =>
          logger.debug(s"Removing student $studentId $studentName distribution, as student chose a full-day topic.")
          None
        case (studentId, (studentName, grade, selectedTopics)) if selectedTopics.keySet.contains(unselectedTopicId) =>
          val remainingTopics = selectedTopics.filterNot { case (topicId, _) => topicId == unselectedTopicId }
          logger.debug(s"Removing non-selected topics for student $studentId, remaining topics = $remainingTopics.")
          Some((studentId, (studentName, grade, remainingTopics)))
        case valid => Some(valid)
      }
      studentsSelectedTopicsWithName
    }

  }

  private def excludedTopics(config: InputConfig): Set[TopicId] = config.wFullDayTopics.map(TopicId).toSet

  private def to[A](f: Int => A)(s: String): A = f(s.trim.toInt)

}
