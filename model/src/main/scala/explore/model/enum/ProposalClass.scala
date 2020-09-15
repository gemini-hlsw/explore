// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class ProposalClass(val label: String) extends Product with Serializable

object ProposalClass {
  case object LargeProgram   extends ProposalClass("Large Program Observing at Gemini")
  case object FastTurnaround extends ProposalClass("Fast Turnaround Observing at Gemini")
  case object Queue          extends ProposalClass("Queue Observing at Gemini")
  case object Classical      extends ProposalClass("Classical Observing at Gemini")
  case object Exchange       extends ProposalClass("Exchange Observing at Keck/Subaru")
  case object Intensive      extends ProposalClass("Intensive Program Observing at Subaru")
  case object Other          extends ProposalClass("Other Proposal Types")

  implicit val ProposalClassEnumerated: Enumerated[ProposalClass] =
    Enumerated.of(LargeProgram, FastTurnaround, Queue, Classical, Exchange, Intensive, Other)
}
