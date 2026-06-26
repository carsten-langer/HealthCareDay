package hcd.inout

import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat}
import hcd.model.Metric._
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
      writer.writeRow(List("GlobalMetric", "MetricWorkshops", "MetricStudent1", "..."))
    }
    val otherFiles = Seq(workshopAssignmentsCsvFile, studentAssignmentsCsvFile)
    otherFiles.foreach(_.delete())
    otherFiles.foreach(_.createNewFile())
  }

  private type WriteDistribution = CmdLineConfig => (Topics, Workshops, StudentsNameSelectedTopics) => WorkshopAssignments => Unit

  def writeDistribution: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) =>
          Seq(appendMetric, writeWorkshopAssignments, writeStudentAssignments)
            .foreach(f => f(config)(topics, workshops, studentsNameSelectedTopics)(workshopAssignments))

  private def appendMetric: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          val studentsSelectedTopics = studentsSelectedTopicsFrom(studentsNameSelectedTopics)
          val globalMetric = metricGlobal(topics, workshops, studentsSelectedTopics)(workshopAssignments)
          val workshopsMetric = metricWorkshops(workshopAssignments)
          val orderedStudentsMetrics = orderedMetricsStudents(topics, workshops, studentsSelectedTopics)(workshopAssignments)
          val _ = Using(CSVWriter.open(metricCsvFile, append = true)(csvFormat(config))) { writer =>
            writer.writeRow(List(globalMetric.m, workshopsMetric.m) ++ orderedStudentsMetrics.map(_.m))
          }
        }

  private def writeWorkshopAssignments: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          val _ = Using(CSVWriter.open(workshopAssignmentsCsvFile)(csvFormat(config))) { writer =>
            writer.writeRow(List("WorkshopId", "TopicId", "TopicName", "TimeSlot", "Category", "Preassigned",
              "OnlyVoluntary", "Grades", "Seats", "UsedSeats", "LeftSeats", "WorkshopMetric",
              "Student1", "Student2", "..."))
            workshopAssignments
              .toList
              .sortBy { case (WorkshopId(id), _) => id }
              .foreach { case (workshopId, unsortedStudentIds) =>
                val (topicId, timeSlot, unorderedGrades, Seats(seats)) = workshops(workshopId)
                val (topicName, category, preassigned, onlyVoluntary) = topics(topicId)
                val grades = unorderedGrades.map(_.grade).toList.sorted.mkString(",")
                val usedSeats = unsortedStudentIds.size
                val leftSeats = seats - usedSeats
                val workshopMetric = metricWorkshop(usedSeats).m
                val studentIds = unsortedStudentIds.toList.sortBy(_.id)
                val students = studentIds.map { studentId =>
                  val (studentName, _, _) = studentsNameSelectedTopics(studentId)
                  s"${studentId.id}, $studentName"
                }
                writer.writeRow(List[Any](
                  workshopId.id,
                  topicId.id,
                  topicName,
                  timeSlot.ts,
                  category,
                  preassigned,
                  onlyVoluntary,
                  grades,
                  seats,
                  usedSeats,
                  leftSeats,
                  workshopMetric,
                ) ++ students)
              }
          }
        }

  private def writeStudentAssignments: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          val studentAssignments = studentAssignmentsFrom(workshopAssignments)
          val _ = Using(CSVWriter.open(studentAssignmentsCsvFile)(csvFormat(config))) { writer =>
            writer.writeRow(List(
              "StudentId", "StudentName", "Grade", "Metric",
              "First", "OneOfFirstTwo", "OneOfFirstThree", "bothFirstTwo",
              "TS1Prio", "TS2Prio", "TS3Prio",
              "TopicId1", "WorkshopId1", "TopicName1", "Category1", "Preassigned1", "OnlyVoluntary1",
              "TopicId2", "WorkshopId2", "TopicName2", "Category2", "Preassigned2", "OnlyVoluntary2",
              "TopicId3", "WorkshopId3", "TopicName3", "Category3", "Preassigned3", "OnlyVoluntary3",
            ))
            studentAssignments
              .toList
              .sortBy { case (StudentId(id), _) => id }
              .foreach { case (studentId, assignedWorkshopIds) =>
                val (studentName, grade, selectedTopics) = studentsNameSelectedTopics(studentId)
                val metric = metricStudent(topics, workshops)(assignedWorkshopIds, selectedTopics)
                val assignedTopicIds = assignedWorkshopIds.map(workshops).map { case (topicId, _, _, _) => topicId }
                val first = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio == 1
                }
                val oneOfFirstTwo = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio <= 2
                }
                val oneOfFirstThree = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio <= 3
                }
                val bothFirstTwo = selectedTopics.isEmpty || (
                  selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                    assignedTopicIds.contains(topicId) && prio == 1
                  } && selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                    assignedTopicIds.contains(topicId) && prio == 2
                  })
                val assignedWorkshopsWithPrios = assignedWorkshopIds.map { workshopId =>
                  val (topicId, timeSlot, _, _) = workshops(workshopId)
                  val (topicName, category, preassigned, onlyVoluntary) = topics(topicId)
                  val selectionPriority = selectedTopics.get(topicId) match {
                    case Some(selectionPriority) => selectionPriority
                    case None if selectedTopics.isEmpty => unselectedPrio
                    case None => unwantedSelectionPrio
                  }
                  (timeSlot, selectionPriority.prio, List[Any](topicId.id, workshopId.id, topicName, category, preassigned, onlyVoluntary))
                }.toList.sortBy { case (timeSlot, _, _) => timeSlot.ts }
                val assignedPrios = assignedWorkshopsWithPrios.map { case (_, prio, _) => prio }
                val assignedWorkshopsWithoutPrios = assignedWorkshopsWithPrios.flatMap { case (_, _, list) => list }
                val assignedWorkshops = assignedPrios ++ assignedWorkshopsWithoutPrios
                writer.writeRow(List[Any](
                  studentId.id,
                  studentName,
                  grade.grade,
                  metric.m,
                  first,
                  oneOfFirstTwo,
                  oneOfFirstThree,
                  bothFirstTwo,
                ) ++ assignedWorkshops)
              }
          }
        }

  private def csvFormat(config: CmdLineConfig) =
    new DefaultCSVFormat {
      override val delimiter: Char = config.oDelimiter
    }

}
