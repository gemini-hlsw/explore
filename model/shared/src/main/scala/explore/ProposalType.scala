// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.model.Partner
import lucuma.core.enums.ToOActivation
import explore.model.enums.ScienceSubtype
import lucuma.core.model.IntPercent
import eu.timepit.refined.cats.given
import lucuma.core.util.TimeSpan

// Define the ProposalType trait
sealed trait ProposalType derives Eq {
  val scienceSubtype: ScienceSubtype
}

object ProposalType:

  // Define the Classical case class implementing ProposalType
  case class Classical(
    scienceSubtype: ScienceSubtype,
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType
      derives Eq

  // Define the DemoScience case class implementing ProposalType
  case class DemoScience(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType
      derives Eq

  // Define the DirectorsTime case class implementing ProposalType
  case class DirectorsTime(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType
      derives Eq

  // Define the FastTurnaround case class implementing ProposalType
  case class FastTurnaround(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent,
    piAffiliation:  Option[Partner]
  ) extends ProposalType
      derives Eq

  // Define the LargeProgram case class implementing ProposalType
  case class LargeProgram(
    scienceSubtype:      ScienceSubtype,
    toOActivation:       ToOActivation,
    minPercentTime:      IntPercent,
    minPercentTotalTime: IntPercent,
    totalTime:           TimeSpan
  ) extends ProposalType
      derives Eq

  // Define the PoorWeather case class implementing ProposalType
  case class PoorWeather(
    scienceSubtype: ScienceSubtype
  ) extends ProposalType
      derives Eq

  // Define the Queue case class implementing ProposalType
  case class Queue(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType
      derives Eq

  // Define the SystemVerification case class implementing ProposalType
  case class SystemVerification(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType
      derives Eq
