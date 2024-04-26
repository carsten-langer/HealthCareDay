package hcd.algorithms

import enumeratum.EnumEntry.Lowercase
import enumeratum._
import hcd.model.InitiallySeededStoppableDistributionAlgorithmSavingIntermediateStates

sealed abstract class Algorithm(val distributionAlgorithm: InitiallySeededStoppableDistributionAlgorithmSavingIntermediateStates) extends EnumEntry with Lowercase

case object Algorithm extends Enum[Algorithm] {

  case object RandomRoundRobin extends Algorithm(distributionAlgorithm = randomroundrobin.Algorithm.distributionAlgorithm)

  val values: IndexedSeq[Algorithm] = findValues

  implicit val algorithmRead: scopt.Read[Algorithm] = scopt.Read.reads(Algorithm.withName)

}


