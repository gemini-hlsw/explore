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
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProposalReference
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Iso
import monocle.Lens

import java.time.Duration
import java.time.LocalDateTime

case class Proposal(
  callId:       Option[CallForProposals.Id],
  title:        Option[NonEmptyString],
  category:     Option[TacCategory],
  abstrakt:     Option[NonEmptyString],
  proposalType: Option[ProposalType],
  reference:    Option[ProposalReference]
) derives Eq:
  def deadline(cfps: List[CallForProposal], piPartner: Option[PartnerLink]): Option[Timestamp] =
    cfps.find(cfp => callId.exists(_ === cfp.id)).flatMap(_.deadline(piPartner))

object Proposal:
  val callId: Lens[Proposal, Option[CallForProposals.Id]]  =
    Focus[Proposal](_.callId)
  val title: Lens[Proposal, Option[NonEmptyString]]        =
    Focus[Proposal](_.title)
  val category: Lens[Proposal, Option[TacCategory]]        =
    Focus[Proposal](_.category)
  val abstrakt: Lens[Proposal, Option[NonEmptyString]]     =
    Focus[Proposal](_.abstrakt)
  val proposalType: Lens[Proposal, Option[ProposalType]]   =
    Focus[Proposal](_.proposalType)
  val callWithType: Lens[Proposal, Option[ProposalType]]   =
    Focus[Proposal](_.proposalType)
  val reference: Lens[Proposal, Option[ProposalReference]] =
    Focus[Proposal](_.reference)

  given Decoder[Proposal] = c =>
    for {
      callId   <-
        c.downField("call").downField("id").success.traverse(_.as[Option[CallForProposals.Id]])
      title    <- c.downField("title").as[Option[NonEmptyString]]
      category <- c.downField("category").as[Option[TacCategory]]
      abstrakt <- c.downField("abstract").as[Option[NonEmptyString]]
      pte      <- c.downField("type").as[Option[ProposalType]]
      r        <-
        c.downField("reference")
          .downField("label")
          .success
          .traverse(_.as[Option[ProposalReference]])
    } yield Proposal(callId.flatten, title, category, abstrakt, pte, r.flatten)

  val Default = Proposal(None, None, None, None, None, None)

  def deadlineAndTimeLeft(now: Timestamp, deadline: Timestamp): (String, Option[String]) = {
    val deadlineLDT: LocalDateTime = deadline.toLocalDateTime
    val nowLDT: LocalDateTime      = now.toLocalDateTime
    val diff: Duration             = Duration.between(nowLDT, deadlineLDT)
    val deadlineStr: String        = deadlineString(deadline)
    if (diff.isNegative) (deadlineStr, None)
    else
      val left = Constants.DurationLongWithSecondsFormatter(diff)
      (deadlineStr, left.some)
  }

  def deadlineString(deadline: Timestamp): String = {
    val deadlineLDT = deadline.toLocalDateTime
    s"${Constants.GppDateFormatter.format(deadlineLDT)} ${Constants.GppTimeTZFormatterWithZone.format(deadlineLDT)}"
  }
