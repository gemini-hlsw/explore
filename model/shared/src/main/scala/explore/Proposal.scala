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

case class Proposal(
  callId:       Option[CallForProposals.Id],
  title:        Option[NonEmptyString],
  category:     Option[TacCategory],
  abstrakt:     Option[NonEmptyString],
  proposalType: Option[ProposalType]
) derives Eq

object Proposal:
  val callId: Lens[Proposal, Option[CallForProposals.Id]] =
    Focus[Proposal](_.callId)
  val title: Lens[Proposal, Option[NonEmptyString]]       =
    Focus[Proposal](_.title)
  val category: Lens[Proposal, Option[TacCategory]]       =
    Focus[Proposal](_.category)
  val abstrakt: Lens[Proposal, Option[NonEmptyString]]    =
    Focus[Proposal](_.abstrakt)
  val proposalType: Lens[Proposal, Option[ProposalType]]  =
    Focus[Proposal](_.proposalType)
  val callWithType: Lens[Proposal, Option[ProposalType]]  =
    Focus[Proposal](_.proposalType)

  given Decoder[Proposal] = c =>
    for {
      callId   <-
        c.downField("call").downField("id").success.traverse(_.as[Option[CallForProposals.Id]])
      title    <- c.downField("title").as[Option[NonEmptyString]]
      category <- c.downField("category").as[Option[TacCategory]]
      abstrakt <- c.downField("abstract").as[Option[NonEmptyString]]
      pte      <- c.downField("type").as[Option[ProposalType]]
    } yield Proposal(callId.flatten, title, category, abstrakt, pte)

  val Default = Proposal(None, None, None, None, None)
