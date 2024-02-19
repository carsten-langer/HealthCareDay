package hcd.inout

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import com.typesafe.scalalogging.StrictLogging
import hcd.model._
import io.cvbio.collection.mutable.bimap.BiMap

import scala.util.Using

object InputCsvConversion extends StrictLogging {

  private final case class Workshop(topicId: TopicId, timeSflot: TimeSlot, category: Category, grades: Set[Grade], seats: Seats)

  def readHcdWorkshopPlanning(config: InputConfig): Unit = {

    val csvFormat = new DefaultCSVFormat {
      override val delimiter: Char = config.wDelimiter
    }

    def category(categoryStr: String): Category = categoryStr.trim match {
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

    def maybeWorkshop(topicIdStr: String, timeSlot: TimeSlot, categoryStr: String, gradesStr: String, seatsStr: String): Option[Workshop] =
      for {
        seats <- maybeSeats(seatsStr)
        grades <- maybeGrades(gradesStr)
      } yield Workshop(to(TopicId)(topicIdStr), timeSlot, category(categoryStr), grades, seats)

    val _ = Using(CSVReader.open(config.wFile)(csvFormat)) { reader =>
      reader.all().slice(config.wRowsToSkip, config.wRowsToSkip + config.wNoTopics).map { columns =>
        val topicId = columns(config.wColTopicId - 1)
        val category = columns(config.wColCategory - 1)
        val name = columns(config.wColName - 1)
        val grades1 = columns(config.wColGrades1 - 1)
        val seats1 = columns(config.wColSeats1 - 1)
        val grades2 = columns(config.wColGrades2 - 1)
        val seats2 = columns(config.wColSeats2 - 1)
        val grades3 = columns(config.wColGrades3 - 1)
        val seats3 = columns(config.wColSeats3 - 1)
        val ws1 = maybeWorkshop(topicId, FirstTimeSlot, category, grades1, seats1)
        val ws2 = maybeWorkshop(topicId, SecondTimeSlot, category, grades2, seats2)
        val ws3 = maybeWorkshop(topicId, ThirdTimeSlot, category, grades3, seats3)

        logger.info(s"$topicId, $category, $name, g1=$grades1, s1=$seats1, g2=$grades2, s2=$seats2, g3=$grades3, s3=$seats3")
        logger.debug(s"$ws1, $ws2, $ws3")
      }
    }

  }

  def readHcdStudentTopicSelection(config: InputConfig): Unit = {

    val csvFormat = new DefaultCSVFormat {
      override val delimiter: Char = config.sDelimiter
    }

    val studentsSelectedTopics = Using(CSVReader.open(config.sFile)(csvFormat)) { reader =>
      reader.all().slice(config.sRowsToSkip, config.sRowsToSkip + config.sNoStudents).map { columns =>
        val studentId = to(StudentId)(columns(config.sColStudentId - 1))
        val grade = to(Grade)(columns(config.sColGrade - 1))
        val selectedTopics = BiMap.from(Range.inclusive(1, 6)
          .map { prio =>
            val topicId = to(TopicId)(columns(config.sColFirstSelection - 1 + prio - 1))
            val selectionPriority = SelectionPriority(prio)
            topicId -> selectionPriority
          })
        logger.info(s"$studentId, $grade, $selectedTopics")
        studentId -> (grade, selectedTopics)
      }.toMap
    }
    logger.debug(studentsSelectedTopics.toString)

  }

  private def to[A](f: Int => A)(s: String): A = f(s.trim.toInt)

}
