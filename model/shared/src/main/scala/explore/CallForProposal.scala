// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester
import lucuma.core.util.Enumerated
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens

case class CallPartner(partner: Partner) derives Eq, Decoder

case class CallForProposal(
  id:       CallForProposals.Id,
  semester: Semester,
  title:    NonEmptyString,
  cfpType:  CallForProposalsType,
  partners: List[CallPartner]
) derives Eq,
      Decoder

object CallForProposal:
  val id: Lens[CallForProposal, CallForProposals.Id] =
    Focus[CallForProposal](_.id)
