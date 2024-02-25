package hcd.algorithms.randomroundrobin

import hcd.algorithms.fixtureSymmetricWorkshopsFor
import hcd.algorithms.randomroundrobin.Algorithm.distributionAlgorithm
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

    }

  }

}
