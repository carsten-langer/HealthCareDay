package hcd.algorithms.randomroundrobin

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
        distributionAlgorithm(Map.empty, Map.empty)(Map.empty).value shouldBe empty
      }

    }

  }

}
