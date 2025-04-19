// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.TacCategory
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProposalReference
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Iso
import monocle.Lens

import java.time.Duration
import java.time.LocalDateTime

case class Proposal(
  call:         Option[CallForProposal],
  category:     Option[TacCategory],
  proposalType: Option[ProposalType],
  reference:    Option[ProposalReference]
) derives Eq:
  def deadline(piPartner: Option[PartnerLink]): Option[Timestamp] =
    call.flatMap(_.deadline(piPartner))

object Proposal:
  val call: Lens[Proposal, Option[CallForProposal]]        =
    Focus[Proposal](_.call)
  val category: Lens[Proposal, Option[TacCategory]]        =
    Focus[Proposal](_.category)
  val proposalType: Lens[Proposal, Option[ProposalType]]   =
    Focus[Proposal](_.proposalType)
  val callWithType: Lens[Proposal, Option[ProposalType]]   =
    Focus[Proposal](_.proposalType)
  val reference: Lens[Proposal, Option[ProposalReference]] =
    Focus[Proposal](_.reference)

  given Decoder[Proposal] = c =>
    for {
      call     <- c.downField("call").as[Option[CallForProposal]]
      category <- c.downField("category").as[Option[TacCategory]]
      pte      <- c.downField("type").as[Option[ProposalType]]
      r        <-
        c.downField("reference")
          .downField("label")
          .success
          .traverse(_.as[Option[ProposalReference]])
    } yield Proposal(call, category, pte, r.flatten)

  val Default = Proposal(None, None, None, None)

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
