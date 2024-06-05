// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester
import lucuma.core.util.Enumerated
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens

// TODO move to lucuma-core
enum CallForProposalType(val tag: String) derives Enumerated:
  case DemoScience        extends CallForProposalType("DemoScience")
  case DirectorsTime      extends CallForProposalType("DirectorsTime")
  case FastTurnaround     extends CallForProposalType("FastTurnaround")
  case LargeProgram       extends CallForProposalType("LargeProgram")
  case PoorWeather        extends CallForProposalType("PoorWeather")
  case RegularSemester    extends CallForProposalType("RegularSemester")
  case SystemVerification extends CallForProposalType("SystemVerification")

  def subTypes: NonEmptyList[ScienceSubtype] =
    this match
      case CallForProposalType.LargeProgram       => NonEmptyList.of(ScienceSubtype.LargeProgram)
      case CallForProposalType.FastTurnaround     => NonEmptyList.of(ScienceSubtype.FastTurnaround)
      case CallForProposalType.RegularSemester    =>
        NonEmptyList.of(ScienceSubtype.Classical, ScienceSubtype.Queue)
      case CallForProposalType.SystemVerification =>
        NonEmptyList.of(ScienceSubtype.SystemVerification)
      case CallForProposalType.PoorWeather        => NonEmptyList.of(ScienceSubtype.PoorWeather)
      case CallForProposalType.DemoScience        => NonEmptyList.of(ScienceSubtype.DemoScience)
      case CallForProposalType.DirectorsTime      => NonEmptyList.of(ScienceSubtype.DirectorsTime)

case class CallPartner(partner: Partner) derives Eq, Decoder

case class CallForProposal(
  id:       CallForProposals.Id,
  semester: Semester,
  title:    NonEmptyString,
  cfpType:  CallForProposalType,
  partners: List[CallPartner]
) derives Eq,
      Decoder

object CallForProposal:
  val id: Lens[CallForProposal, CallForProposals.Id] =
    Focus[CallForProposal](_.id)
