// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.TacCategory

import monocle.Focus
import monocle.Lens
import io.circe.Decoder
import io.circe.refined.*
import eu.timepit.refined.cats.given
import lucuma.core.model.CallForProposals
import lucuma.odb.json.all.*

case class Proposal(
  cfpId:        Option[CallForProposals.Id],
  title:        Option[NonEmptyString],
  category:     Option[TacCategory],
  abstrakt:     Option[NonEmptyString],
  proposalType: Option[ProposalType]
) derives Eq

object Proposal:
  val cfpId: Lens[Proposal, Option[CallForProposals.Id]] =
    Focus[Proposal](_.cfpId)
  val title: Lens[Proposal, Option[NonEmptyString]]      =
    Focus[Proposal](_.title)
  val category: Lens[Proposal, Option[TacCategory]]      =
    Focus[Proposal](_.category)
  val abstrakt: Lens[Proposal, Option[NonEmptyString]]   =
    Focus[Proposal](_.abstrakt)
  val proposalType: Lens[Proposal, Option[ProposalType]] =
    Focus[Proposal](_.proposalType)

  given Decoder[Proposal] = c =>
    for {
      cfpId    <- c.downField("call").field("id").as[Option[CallForProposals.Id]]
      title    <- c.downField("title").as[Option[NonEmptyString]]
      category <- c.downField("category").as[Option[TacCategory]]
      abstrakt <- c.downField("abstract").as[Option[NonEmptyString]]
    } yield Proposal(cfpId, title, category, abstrakt, None)

  val Default = Proposal(None, None, None, None, None)
