package hcd.algorithms.randomroundrobin

import hcd.algorithms.fixtureSymmetricWorkshopsFor
import hcd.algorithms.randomroundrobin.Algorithm.distributionAlgorithm
import hcd.model.{SelectionPriority, StudentId, TopicId, WorkshopId}
import io.cvbio.collection.mutable.bimap.BiMap
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AlgorithmSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues {

  "Algorithm" should {

    "provide a distributionAlgorithm" which {

      "finds the empty distribution if there are neither topics/workshops nor students selections to distribute" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 0)

        distributionAlgorithm(Map.empty, Map.empty)(Map.empty).value shouldBe empty
        distributionAlgorithm(Map.empty, f.workshops)(Map.empty).value shouldBe empty
        distributionAlgorithm(f.topics, f.workshops)(Map.empty).value shouldBe empty
      }

      "finds the empty distribution if there are workshops but no students selections to distribute" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val expectedWorkshopAssignments = f.workshops.view.mapValues(_ => Set.empty).toMap

        distributionAlgorithm(Map.empty, f.workshops)(Map.empty).value shouldEqual expectedWorkshopAssignments
        distributionAlgorithm(f.topics, f.workshops)(Map.empty).value shouldEqual expectedWorkshopAssignments
      }

      "assigns a workshop to a student which has not selected any topic" in {
        val f = fixtureSymmetricWorkshopsFor(noTopics = 1)
        val student1 = StudentId(1)
        val studentsSelectedTopics = Map(student1 -> (f.grade, BiMap.empty[TopicId, SelectionPriority]))
        // student1 has no selection, thus gets assigned next best workshop
        val expectedWorkshopAssignments = Map(
          WorkshopId(0) -> Set(student1), WorkshopId(1) -> Set.empty, WorkshopId(2) -> Set.empty, // TopicId(0)
        )

        distributionAlgorithm(f.topics, f.workshops)(studentsSelectedTopics).value shouldEqual expectedWorkshopAssignments
      }

    }

  }

}
