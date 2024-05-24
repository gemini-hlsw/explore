// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import lucuma.core.model.IntPercent
// import lucuma.core.model.ProposalClass
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

// enum ProposalClassType(val label: String) derives Enumerated:
//   case LargeProgram       extends ProposalClassType("Large Program Observing at Gemini")
//   case FastTurnaround     extends ProposalClassType("Fast Turnaround Observing at Gemini")
//   case Queue              extends ProposalClassType("Queue Observing at Gemini")
//   case Classical          extends ProposalClassType("Classical Observing at Gemini")
//   case Exchange           extends ProposalClassType("Exchange Observing at Keck/Subaru")
//   case Intensive          extends ProposalClassType("Intensive Program Observing at Subaru")
//   case DemoScience        extends ProposalClassType("Demo Science")
//   case DirectorsTime      extends ProposalClassType("Directors Time")
//   case PoorWeather        extends ProposalClassType("Poor Weather")
//   case SystemVerification extends ProposalClassType("System Verification")
//
//   private val tag = label
//
//   def toProposalClass(
//     minPctTime:      IntPercent,
//     minPctTotalTime: IntPercent,
//     totalTime:       TimeSpan
//   ): ProposalClass = {
//     import ProposalClassType.*
//     this match {
//       case LargeProgram       => ProposalClass.LargeProgram(minPctTime, minPctTotalTime, totalTime)
//       case FastTurnaround     => ProposalClass.FastTurnaround(minPctTime)
//       case Queue              => ProposalClass.Queue(minPctTime)
//       case Classical          => ProposalClass.Classical(minPctTime)
//       case Exchange           => ProposalClass.Exchange(minPctTime)
//       case Intensive          => ProposalClass.Intensive(minPctTime, minPctTotalTime, totalTime)
//       case DemoScience        => ProposalClass.DemoScience(minPctTime)
//       case DirectorsTime      => ProposalClass.DirectorsTime(minPctTime)
//       case PoorWeather        => ProposalClass.PoorWeather(minPctTime)
//       case SystemVerification => ProposalClass.SystemVerification(minPctTime)
//     }
//   }
//
// object ProposalClassType:
//   def fromProposalClass(proposalClass: ProposalClass): ProposalClassType = proposalClass match {
//     case ProposalClass.Intensive(_, _, _)    => Intensive
//     case ProposalClass.PoorWeather(_)        => PoorWeather
//     case ProposalClass.DirectorsTime(_)      => DirectorsTime
//     case ProposalClass.SystemVerification(_) => SystemVerification
//     case ProposalClass.Classical(_)          => Classical
//     case ProposalClass.LargeProgram(_, _, _) => LargeProgram
//     case ProposalClass.DemoScience(_)        => DemoScience
//     case ProposalClass.Exchange(_)           => Exchange
//     case ProposalClass.Queue(_)              => Queue
//     case ProposalClass.FastTurnaround(_)     => FastTurnaround
//   }
//
//   given Display[ProposalClassType] = Display.byShortName(_.label)
