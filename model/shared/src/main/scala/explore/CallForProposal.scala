// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.Instrument
import lucuma.core.enums.Partner
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.CallForProposals
import lucuma.core.model.PartnerLink
import lucuma.core.model.Semester
import lucuma.core.util.DateInterval
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.odb.json.limits.decoder.given
import lucuma.odb.json.time.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens

case class CallPartner(
  partner:            Partner,
  submissionDeadline: Option[Timestamp]
) derives Eq,
      Decoder

case class CallForProposal(
  id:                 CallForProposals.Id,
  semester:           Semester,
  title:              NonEmptyString,
  cfpType:            CallForProposalsType,
  partners:           List[CallPartner],
  allowsNonPartnerPi: Boolean,
  nonPartnerDeadline: Option[Timestamp],
  instruments:        List[Instrument],
  coordinateLimits:   CallCoordinatesLimits,
  active:             DateInterval
) derives Eq,
      Decoder:

  def deadline(piPartner: Option[PartnerLink]): Either[String, Timestamp] =
    // piPartner is only None if there is no pi, which should never happen
    piPartner.fold("No PI for this program.".asLeft):
      _.fold(
        "Select PI's partner to show CfP deadline.".asLeft,
        nonPartnerDeadline.fold("Non-partner PI not allowed for this CfP.".asLeft)(
          _.asRight
        ),
        _ =>
          partners
            .find(p => piPartner.flatMap(_.partnerOption).exists(_ === p.partner))
            .fold("PI partner not valid for this CfP.".asLeft)(
              _.submissionDeadline
                .fold("PI partner deadline not set for this CfP.".asLeft)(_.asRight)
            )
      )

object CallForProposal:
  val id: Lens[CallForProposal, CallForProposals.Id] =
    Focus[CallForProposal](_.id)
