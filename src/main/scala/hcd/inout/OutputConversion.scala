package hcd.inout

import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat}
import hcd.model.Metric._
import hcd.model.SelectionPriority.{UnselectedPrio, UnwantedSelectionPrio}
import hcd.model._

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets.UTF_8
import scala.util.Using

object OutputConversion {

  val studentsJsonFile = new File("Students.json")
  val workshopsJsonFile = new File("Workshops.json")
  val workshopAssignmentsJsonFile = new File("WorkshopAssignments.json")
  val metricCsvFile = new File("Metric.csv")
  val workshopAssignmentsCsvFile = new File("WorkshopAssignments.csv")
  val studentAssignmentsCsvFile = new File("StudentAssignments.csv")

  def writeNonDistributionJsonFiles(topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics): Unit = {
    writeStudentsJson(studentsNameSelectedTopics)
    writeWorkshopsJson(topics, workshops)
  }

  def initWriteDistribution(config: CmdLineConfig): Unit = {
    Using(CSVWriter.open(metricCsvFile)(csvFormat(config))) { writer =>
      writer.writeRow(List("GlobalMetric", "MetricWorkshops", "MetricStudent1", "..."))
    }
    val otherFiles = Seq(workshopAssignmentsCsvFile, studentAssignmentsCsvFile, workshopAssignmentsJsonFile)
    otherFiles.foreach(_.delete())
    otherFiles.foreach(_.createNewFile())
  }

  private type WriteDistribution = CmdLineConfig => (Topics, Workshops, StudentsNameSelectedTopics) => WorkshopAssignments => Unit

  def writeDistribution: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          Seq(appendMetric, writeWorkshopAssignmentsCsv, writeStudentAssignmentsCsv)
            .foreach(f => f(config)(topics, workshops, studentsNameSelectedTopics)(workshopAssignments))
          writeWorkshopAssignmentsJson(workshopAssignments)
        }

  private def writeStudentsJson(studentsNameSelectedTopics: StudentsNameSelectedTopics): Unit = {
    val _ = Using(new PrintWriter(studentsJsonFile, UTF_8)) { writer =>
      val studentJsons = studentsNameSelectedTopics.toList
        .sortBy { case (studentId, _) => studentId.id }
        .map { case (StudentId(id), (studentName, _, className, _, _)) =>
          s"""{"studentId": $id, "name": "$studentName", "studentClass": "$className"}"""
        }
      writer.write(studentJsons.mkString("[\n", ",\n", "\n]"))
    }
  }

  private def writeWorkshopsJson(topics: Topics, workshops: Workshops): Unit = {
    val _ = Using(new PrintWriter(workshopsJsonFile, UTF_8)) { writer =>
      val workshopJsons = workshops.toList
        .sortBy { case (WorkshopId(id), _) => id }
        .map { case (WorkshopId(id), (topicId, timeSlot, _, _, _, Seats(maxSeats))) =>
          val (topicName, _, _, _) = topics(topicId)
          s"""{"workshopId": $id, "name": "$topicName", "timeSlotId": ${timeSlot.ts}, "seats": $maxSeats}"""
        }
      writer.write(workshopJsons.mkString("[\n", ",\n", "\n]"))
    }
  }

  private def appendMetric: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          val studentsSelectedTopics = studentsSelectedTopicsFrom(studentsNameSelectedTopics)
          val globalMetric = metricGlobal(topics, workshops, studentsSelectedTopics)(workshopAssignments)
          val workshopsMetric = metricWorkshops(workshops)(workshopAssignments)
          val orderedStudentsMetrics = orderedMetricsStudents(topics, workshops, studentsSelectedTopics)(workshopAssignments)
          val _ = Using(CSVWriter.open(metricCsvFile, append = true)(csvFormat(config))) { writer =>
            writer.writeRow(List(globalMetric.m, workshopsMetric.m) ++ orderedStudentsMetrics.map(_.m))
          }
        }

  private def writeWorkshopAssignmentsCsv: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          val _ = Using(CSVWriter.open(workshopAssignmentsCsvFile)(csvFormat(config))) { writer =>
            writer.writeRow(List("WorkshopId", "TopicId", "TopicName", "TimeSlot", "Category", "Preassigned",
              "OnlyVoluntary", "Sexes", "Grades", "MinSeats", "MaxSeats", "UsedSeats", "LeftSeats", "WorkshopMetric",
              "Student1", "Student2", "..."))
            workshopAssignments
              .toList
              .sortBy { case (WorkshopId(id), _) => id }
              .foreach { case (workshopId, unsortedStudentIds) =>
                val (topicId, timeSlot, unorderedSexes, unorderedGrades, Seats(minSeats), Seats(maxSeats)) = workshops(workshopId)
                val (topicName, category, preassigned, onlyVoluntary) = topics(topicId)
                val sexes = unorderedSexes.toList.sortBy(_.getClass.getSimpleName).mkString(",")
                val grades = unorderedGrades.map(_.grade).toList.sorted.mkString(",")
                val usedSeats = unsortedStudentIds.size
                val leftSeats = maxSeats - usedSeats
                val workshopMetric = metricWorkshop(workshops)(workshopId, usedSeats).m
                val studentIds = unsortedStudentIds.toList.sortBy(_.id)
                val students = studentIds.map { studentId =>
                  val (studentName, _, _, _, _) = studentsNameSelectedTopics(studentId)
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
                  sexes,
                  grades,
                  minSeats,
                  maxSeats,
                  usedSeats,
                  leftSeats,
                  workshopMetric,
                ) ++ students)
              }
          }
        }

  private def writeStudentAssignmentsCsv: WriteDistribution =
    (config: CmdLineConfig) =>
      (topics: Topics, workshops: Workshops, studentsNameSelectedTopics: StudentsNameSelectedTopics) =>
        (workshopAssignments: WorkshopAssignments) => {
          val _ = Using(CSVWriter.open(studentAssignmentsCsvFile)(csvFormat(config))) { writer =>
            writer.writeRow(List(
              "StudentId", "StudentName", "Sex", "Grade", "Metric",
              "First", "Second", "Third", "Forth", "OneOfFirstTwo", "OneOfFirstThree", "FirstAndSecond", "SecondAndThird",
              "TS1Prio", "TS2Prio", "TS3Prio",
              "TopicId1", "WorkshopId1", "TopicName1", "Category1", "Preassigned1", "OnlyVoluntary1",
              "TopicId2", "WorkshopId2", "TopicName2", "Category2", "Preassigned2", "OnlyVoluntary2",
              "TopicId3", "WorkshopId3", "TopicName3", "Category3", "Preassigned3", "OnlyVoluntary3",
            ))
            studentAssignmentsFrom(workshopAssignments)
              .toList
              .sortBy { case (StudentId(id), _) => id }
              .foreach { case (studentId, assignedWorkshopIds) =>
                val (studentName, sex, _, grade, selectedTopics) = studentsNameSelectedTopics(studentId)
                val metric = metricStudent(topics, workshops)(studentId, assignedWorkshopIds, selectedTopics)
                val assignedTopicIds = assignedWorkshopIds.map(workshops).map { case (topicId, _, _, _, _, _) => topicId }
                val first = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio == 1
                }
                val second = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio == 2
                }
                val third = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio == 3
                }
                val forth = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio == 4
                }
                val oneOfFirstTwo = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio <= 2
                }
                val oneOfFirstThree = selectedTopics.isEmpty || selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                  assignedTopicIds.contains(topicId) && prio <= 3
                }
                val firstAndSecond = selectedTopics.isEmpty || (
                  selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                    assignedTopicIds.contains(topicId) && prio == 1
                  } && selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                    assignedTopicIds.contains(topicId) && prio == 2
                  })
                val secondAndThird = selectedTopics.isEmpty || (
                  selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                    assignedTopicIds.contains(topicId) && prio == 2
                  } && selectedTopics.exists { case (topicId, SelectionPriority(prio)) =>
                    assignedTopicIds.contains(topicId) && prio == 3
                  })
                val assignedWorkshopsWithPrios = assignedWorkshopIds.map { workshopId =>
                  val (topicId, timeSlot, _, _, _, _) = workshops(workshopId)
                  val (topicName, category, preassigned, onlyVoluntary) = topics(topicId)
                  val selectionPriority = selectedTopics.get(topicId) match {
                    case Some(selectionPriority) => selectionPriority
                    case None if selectedTopics.isEmpty => UnselectedPrio
                    case None => UnwantedSelectionPrio
                  }
                  (timeSlot, selectionPriority.prio, List[Any](topicId.id, workshopId.id, topicName, category, preassigned, onlyVoluntary))
                }.toList.sortBy { case (timeSlot, _, _) => timeSlot.ts }
                val assignedPrios = assignedWorkshopsWithPrios.map { case (_, prio, _) => prio }
                val assignedWorkshopsWithoutPrios = assignedWorkshopsWithPrios.flatMap { case (_, _, list) => list }
                val assignedWorkshops = assignedPrios ++ assignedWorkshopsWithoutPrios
                writer.writeRow(List[Any](
                  studentId.id,
                  studentName,
                  sex,
                  grade.grade,
                  metric.m,
                  first,
                  second,
                  third,
                  forth,
                  oneOfFirstTwo,
                  oneOfFirstThree,
                  firstAndSecond,
                  secondAndThird,
                ) ++ assignedWorkshops)
              }
          }
        }

  private def csvFormat(config: CmdLineConfig) =
    new DefaultCSVFormat {
      override val delimiter: Char = config.oDelimiter
    }

  private def writeWorkshopAssignmentsJson(workshopAssignments: WorkshopAssignments): Unit = {
    val _ = Using(new PrintWriter(workshopAssignmentsJsonFile, UTF_8)) { writer =>
      val workshopAssignmentJsons = workshopAssignments.toList
        .sortBy { case (WorkshopId(id), _) => id }
        .flatMap { case (WorkshopId(wsId), studentIds) =>
          studentIds.toList
            .sortBy(_.id)
            .map(studentId => s"""{"workshopId": $wsId, "studentId": ${studentId.id}}""")
        }
      writer.write(workshopAssignmentJsons.mkString("[\n", ",\n", "\n]"))
    }
  }

}
