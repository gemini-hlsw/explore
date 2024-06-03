// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.TacCategory
import lucuma.core.model.CallForProposals
import monocle.Focus
import monocle.Lens
import monocle.Iso

case class ProposalCall(cfpId: CallForProposals.Id, cfpType: Option[CallForProposalType]) derives Eq

object ProposalCall:
  val cfpId: Lens[ProposalCall, CallForProposals.Id]           =
    Focus[ProposalCall](_.cfpId)
  val cfpType: Lens[ProposalCall, Option[CallForProposalType]] =
    Focus[ProposalCall](_.cfpType)

  given Decoder[ProposalCall] = c =>
    for {
      cfpId   <- c.downField("id").as[CallForProposals.Id]
      cfpType <- c.downField("type").as[CallForProposalType]
    } yield ProposalCall(cfpId, cfpType.some)

  val Default = Proposal(None, None, None, None, None)

case class Proposal(
  call:         Option[CallForProposal],
  title:        Option[NonEmptyString],
  category:     Option[TacCategory],
  abstrakt:     Option[NonEmptyString],
  proposalType: Option[ProposalType]
) derives Eq

object Proposal:
  // Used to reset the proposal type when the call changes
  extension (cfpType: CallForProposalType)
    def defaultType: ProposalType = cfpType match
      case CallForProposalType.DemoScience        => ProposalType.DemoScience.Default
      case CallForProposalType.DirectorsTime      => ProposalType.DirectorsTime.Default
      case CallForProposalType.FastTurnaround     => ProposalType.FastTurnaround.Default
      case CallForProposalType.LargeProgram       => ProposalType.LargeProgram.Default
      case CallForProposalType.PoorWeather        => ProposalType.PoorWeather.Default
      case CallForProposalType.RegularSemester    => ProposalType.Queue.Default
      case CallForProposalType.SystemVerification => ProposalType.SystemVerification.Default

  val call: Lens[Proposal, Option[CallForProposal]]      =
    Focus[Proposal](_.call)
  val title: Lens[Proposal, Option[NonEmptyString]]      =
    Focus[Proposal](_.title)
  val category: Lens[Proposal, Option[TacCategory]]      =
    Focus[Proposal](_.category)
  val abstrakt: Lens[Proposal, Option[NonEmptyString]]   =
    Focus[Proposal](_.abstrakt)
  val proposalType: Lens[Proposal, Option[ProposalType]] =
    Focus[Proposal](_.proposalType)
  val callWithType: Lens[Proposal, Option[ProposalType]] =
    Focus[Proposal](_.proposalType)
  val proposalTypeConverter: Lens[Proposal, Proposal]    = Iso[Proposal, Proposal] { p =>
    println(s"PRop $p");
    println(p.call.map(_.cfpType.defaultType))
    p.copy(call = p.call, proposalType = p.call.map(_.cfpType.defaultType))
  } { p =>
    println(s"PRop 2 $p");
    println(p.call.map(_.cfpType.defaultType))
    p.copy(call = p.call, proposalType = p.call.map(_.cfpType.defaultType))
  }
  // Focus[Proposal](_.proposalType)

  given Decoder[Proposal] = c =>
    for {
      call     <-
        c.downField("call").as[Option[CallForProposal]]
      title    <- c.downField("title").as[Option[NonEmptyString]]
      category <- c.downField("category").as[Option[TacCategory]]
      abstrakt <- c.downField("abstract").as[Option[NonEmptyString]]
      pte      <- c.downField("type").as[Option[ProposalType]]
    } yield Proposal(call, title, category, abstrakt, pte)

  val Default = Proposal(None, None, None, None, None)
