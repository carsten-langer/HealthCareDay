package hcd.algorithms

import enumeratum.EnumEntry.Lowercase
import enumeratum._
import hcd.model.StoppableDistributionAlgorithm

sealed abstract class Algorithm(val distributionAlgorithm: StoppableDistributionAlgorithm) extends EnumEntry with Lowercase

case object Algorithm extends Enum[Algorithm] {

  //noinspection ScalaUnusedSymbol
  case object FullCombinatoric extends Algorithm(distributionAlgorithm = fullcombinatoric.Algorithm.distributionAlgorithm)

  case object RandomRoundRobin extends Algorithm(distributionAlgorithm = randomroundrobin.Algorithm.distributionAlgorithm)

  val values: IndexedSeq[Algorithm] = findValues

  implicit val algorithmRead: scopt.Read[Algorithm] = scopt.Read.reads(Algorithm.withName)

}


