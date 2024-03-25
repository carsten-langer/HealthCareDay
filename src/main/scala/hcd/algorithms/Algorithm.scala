package hcd.algorithms

import enumeratum.EnumEntry.Lowercase
import enumeratum._
import hcd.model.StoppableDistributionAlgorithmSavingIntermediateStates

sealed abstract class Algorithm(val distributionAlgorithm: StoppableDistributionAlgorithmSavingIntermediateStates) extends EnumEntry with Lowercase

case object Algorithm extends Enum[Algorithm] {

  //noinspection ScalaUnusedSymbol
  case object FullCombinatoric extends Algorithm(distributionAlgorithm = _ => fullcombinatoric.Algorithm.distributionAlgorithm)

  case object RandomRoundRobin extends Algorithm(distributionAlgorithm = randomroundrobin.Algorithm.distributionAlgorithm)

  val values: IndexedSeq[Algorithm] = findValues

  implicit val algorithmRead: scopt.Read[Algorithm] = scopt.Read.reads(Algorithm.withName)

}


