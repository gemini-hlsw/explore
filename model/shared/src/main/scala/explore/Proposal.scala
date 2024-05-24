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
import eu.timepit.refined.cats.given

case class Proposal(
  call:         Option[CallForProposal],
  title:        Option[NonEmptyString],
  category:     Option[TacCategory],
  abstrakt:     Option[NonEmptyString],
  proposalType: Option[ProposalType]
) derives Eq

object Proposal:
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

  given Decoder[Proposal] = ???

  val Default = Proposal(None, None, None, None, None)
