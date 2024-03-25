package hcd.inout

import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat}
import hcd.model.Metric.{metricGlobal, metricStudent, orderedMetricsStudents}
import hcd.model.SelectionPriority.{unselectedPrio, unwantedSelectionPrio}
import hcd.model._

import java.io.File
import scala.util.Using

object OutputCsvConversion {

  val metricCsvFile = new File("Metric.csv")
  val workshopAssignmentsCsvFile = new File("WorkshopAssignments.csv")
  val studentAssignmentsCsvFile = new File("StudentAssignments.csv")

  def initWriteDistribution(config: CmdLineConfig): Unit = {
    Using(CSVWriter.open(metricCsvFile)(csvFormat(config))) { writer =>
      writer.writeRow(List("GlobalMetric", "MetricStudent1", "..."))
    }
    val otherFiles = Seq(workshopAssignmentsCsvFile, studentAssignmentsCsvFile)
    otherFiles.foreach(_.delete())
    otherFiles.foreach(_.createNewFile())
  }

  private type WriteDistribution = CmdLineConfig => (TopicsWithName, Workshops, StudentsSelectedTopicsWithName) => WorkshopAssignments => Unit

  def writeDistribution: WriteDistribution =
    (config: CmdLineConfig) =>
      (topicsWithName: TopicsWithName, workshops: Workshops, studentsSelectedTopicsWithName: StudentsSelectedTopicsWithName) =>
        (workshopAssignments: WorkshopAssignments) =>
          Seq(appendMetric, writeWorkshopAssignments, writeStudentAssignments)
            .foreach(f => f(config)(topicsWithName, workshops, studentsSelectedTopicsWithName)(workshopAssignments))

  private def appendMetric: WriteDistribution =
    (config: CmdLineConfig) =>
      (topicsWithName: TopicsWithName, workshops: Workshops, studentsSelectedTopicsWithName: StudentsSelectedTopicsWithName) =>
        (workshopAssignments: WorkshopAssignments) => {
          val topics = topicsFrom(topicsWithName)
          val studentsSelectedTopics = studentsSelectedTopicsFrom(studentsSelectedTopicsWithName)
          val globalMetric = metricGlobal(topics, workshops, studentsSelectedTopics)(workshopAssignments)
          val orderedStudentsMetrics = orderedMetricsStudents(topics, workshops, studentsSelectedTopics)(workshopAssignments)
          val _ = Using(CSVWriter.open(metricCsvFile, append = true)(csvFormat(config))) { writer =>
            writer.writeRow(List(globalMetric.m) ++ orderedStudentsMetrics.map(_.m))
          }
        }

  private def writeWorkshopAssignments: WriteDistribution =
    (config: CmdLineConfig) =>
      (topicsWithName: TopicsWithName, workshops: Workshops, studentsSelectedTopicsWithName: StudentsSelectedTopicsWithName) =>
        (workshopAssignments: WorkshopAssignments) => {
          val _ = Using(CSVWriter.open(workshopAssignmentsCsvFile)(csvFormat(config))) { writer =>
            writer.writeRow(List("WorkshopId", "TopicId", "TopicName", "TimeSlot", "Category", "Grades",
              "Seats", "UsedSeats", "LeftSeats", "Student1", "Student2", "..."))
            workshopAssignments
              .toList
              .sortBy { case (WorkshopId(id), _) => id }
              .foreach { case (workshopId, unsortedStudentIds) =>
                val (topicId, timeSlot, unorderedGrades, Seats(seats)) = workshops(workshopId)
                val (topicName, category) = topicsWithName(topicId)
                val grades = unorderedGrades.map(_.grade).toList.sorted.mkString(",")
                val usedSeats = unsortedStudentIds.size
                val leftSeats = seats - usedSeats
                val studentIds = unsortedStudentIds.toList.sortBy(_.id)
                val students = studentIds.map { studentId =>
                  val (studentName, _, _) = studentsSelectedTopicsWithName(studentId)
                  s"${studentId.id}, $studentName"
                }
                writer.writeRow(List[Any](
                  workshopId.id,
                  topicId.id,
                  topicName,
                  timeSlot.ts,
                  category,
                  grades,
                  seats,
                  usedSeats,
                  leftSeats
                ) ++ students)
              }
          }
        }

  private def writeStudentAssignments: WriteDistribution =
    (config: CmdLineConfig) =>
      (topicsWithName: TopicsWithName, workshops: Workshops, studentsSelectedTopicsWithName: StudentsSelectedTopicsWithName) =>
        (workshopAssignments: WorkshopAssignments) => {
          val studentAssignments = studentAssignmentsFrom(workshopAssignments)
          val _ = Using(CSVWriter.open(studentAssignmentsCsvFile)(csvFormat(config))) { writer =>
            val topics = topicsFrom(topicsWithName)
            writer.writeRow(List("StudentId", "StudentName", "Grade", "Metric", "OneOfFirstThree",
              "TimeSlot1", "WorkshopId1", "TopicId1", "TopicName1", "Category1", "Prio1",
              "TimeSlot2", "WorkshopId2", "TopicId2", "TopicName2", "Category2", "Prio2",
              "TimeSlot3", "WorkshopId3", "TopicId3", "TopicName3", "Category3", "Prio3",
            ))
            studentAssignments
              .toList
              .sortBy { case (StudentId(id), _) => id }
              .foreach { case (studentId, assignedWorkshopIds) =>
                val (studentName, grade, selectedTopics) = studentsSelectedTopicsWithName(studentId)
                val metric = metricStudent(topics, workshops)(assignedWorkshopIds, selectedTopics)
                val assignedTopicIds = assignedWorkshopIds.map(workshops).map { case (topicId, _, _, _) => topicId }
                val oneOfFirstThree = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio <= 3
                }
                val assignedWorkshops = assignedWorkshopIds.map { workshopId =>
                    val (topicId, timeSlot, _, _) = workshops(workshopId)
                    val (topicName, category) = topicsWithName(topicId)
                    val selectionPriority = selectedTopics.get(topicId) match {
                      case Some(selectionPriority) => selectionPriority
                      case None if selectedTopics.isEmpty => unselectedPrio
                      case None => unwantedSelectionPrio
                    }
                    (timeSlot, List[Any](timeSlot.ts, workshopId.id, topicId.id, topicName, category, selectionPriority.prio))
                  }.toList.sortBy { case (timeSlot, _) => timeSlot.ts }
                  .flatMap { case (_, list) => list }
                writer.writeRow(List[Any](
                  studentId.id,
                  studentName,
                  grade.grade,
                  metric.m,
                  oneOfFirstThree,
                ) ++ assignedWorkshops)
              }
          }
        }

  private def csvFormat(config: CmdLineConfig) =
    new DefaultCSVFormat {
      override val delimiter: Char = config.oDelimiter
    }

}
