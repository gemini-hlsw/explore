// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import lucuma.core.model.IntPercent
import lucuma.core.model.NonNegDuration
import lucuma.core.model.ProposalClass
import lucuma.core.util.Enumerated

sealed abstract class ProposalClassType(val label: String) extends Product with Serializable {
  def toProposalClass(
    minPctTime:      IntPercent,
    minPctTotalTime: IntPercent,
    totalTime:       NonNegDuration
  ): ProposalClass = {
    import ProposalClassType._
    this match {
      case LargeProgram       => ProposalClass.LargeProgram(minPctTime, minPctTotalTime, totalTime)
      case FastTurnaround     => ProposalClass.FastTurnaround(minPctTime)
      case Queue              => ProposalClass.Queue(minPctTime)
      case Classical          => ProposalClass.Classical(minPctTime)
      case Exchange           => ProposalClass.Exchange(minPctTime)
      case Intensive          => ProposalClass.Intensive(minPctTime, minPctTotalTime, totalTime)
      case DemoScience        => ProposalClass.DemoScience(minPctTime)
      case DirectorsTime      => ProposalClass.DirectorsTime(minPctTime)
      case PoorWeather        => ProposalClass.PoorWeather(minPctTime)
      case SystemVerification => ProposalClass.SystemVerification(minPctTime)
    }
  }
}

object ProposalClassType {
  case object LargeProgram       extends ProposalClassType("Large Program Observing at Gemini")
  case object FastTurnaround     extends ProposalClassType("Fast Turnaround Observing at Gemini")
  case object Queue              extends ProposalClassType("Queue Observing at Gemini")
  case object Classical          extends ProposalClassType("Classical Observing at Gemini")
  case object Exchange           extends ProposalClassType("Exchange Observing at Keck/Subaru")
  case object Intensive          extends ProposalClassType("Intensive Program Observing at Subaru")
  case object DemoScience        extends ProposalClassType("Demo Science")
  case object DirectorsTime      extends ProposalClassType("Directors Time")
  case object PoorWeather        extends ProposalClassType("Poor Weather")
  case object SystemVerification extends ProposalClassType("System Verification")

  def fromProposalClass(proposalClass: ProposalClass): ProposalClassType = proposalClass match {
    case ProposalClass.Intensive(_, _, _)    => Intensive
    case ProposalClass.PoorWeather(_)        => PoorWeather
    case ProposalClass.DirectorsTime(_)      => DirectorsTime
    case ProposalClass.SystemVerification(_) => SystemVerification
    case ProposalClass.Classical(_)          => Classical
    case ProposalClass.LargeProgram(_, _, _) => LargeProgram
    case ProposalClass.DemoScience(_)        => DemoScience
    case ProposalClass.Exchange(_)           => Exchange
    case ProposalClass.Queue(_)              => Queue
    case ProposalClass.FastTurnaround(_)     => FastTurnaround
  }

  implicit val ProposalClassTypeEnumerated: Enumerated[ProposalClassType] =
    Enumerated.of(LargeProgram,
                  FastTurnaround,
                  Queue,
                  Classical,
                  Exchange,
                  Intensive,
                  DemoScience,
                  DirectorsTime,
                  PoorWeather,
                  SystemVerification
    )
}
