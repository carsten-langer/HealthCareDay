package hcd.algorithms

import enumeratum.EnumEntry.Lowercase
import enumeratum._
import hcd.model.DistributionAlgorithm

sealed abstract class Algorithm(val distributionAlgorithm: DistributionAlgorithm) extends EnumEntry with Lowercase

case object Algorithm extends Enum[Algorithm] {

  case object FullCombinatoric extends Algorithm(distributionAlgorithm = fullcombinatoric.Algorithm.distributionAlgorithm)

  val values: IndexedSeq[Algorithm] = findValues

  implicit val algorithmRead: scopt.Read[Algorithm] = scopt.Read.reads(Algorithm.withName)

}


