// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
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

  // in reality, should always have a PI
  extension (users: List[ProgramUser])
    private def pi: Option[ProgramUser]            =
      users.find(_.role === ProgramUserRole.Pi)
    private def hasPi(partner: Partner): Boolean   =
      pi.exists(_.partnerLink.exists(_.partnerOption.exists(_ === partner)))
    private def hasUser(partner: Partner): Boolean =
      users.exists(_.partnerLink.exists(_.partnerOption.exists(_ === partner)))

  private def cfPError(users: List[ProgramUser]): Option[String] =
    call.fold("Call for Proposal is required.".some)(cfp =>
      val piAffiliation = users.pi
        .flatMap(_.partnerLink)
        // if no partner link, it's unspecified
        .fold(PartnerLink.HasUnspecifiedPartner)(identity)
      piAffiliation match
        case PartnerLink.HasPartner(partner)   =>
          Option.when(!cfp.partners.exists(_.partner === partner)) {
            "PI partner not valid for this Call for Proposal."
          }
        case PartnerLink.HasNonPartner         =>
          Option.when(!cfp.allowsNonPartnerPi) {
            "Non-partner PI is not allowed for this Call for Proposal."
          }
        // This gets checked in usersAndTimesErrors
        case PartnerLink.HasUnspecifiedPartner => none
    )

  // if this is None, either a CfP has not been selected or they are not required for the proposal type
  private lazy val partnerSplits: Option[List[PartnerSplit]] =
    proposalType.flatMap(pt => ProposalType.partnerSplits.getOption(pt))

  private lazy val validatePartnerSplits: Option[String] =
    partnerSplits.flatMap(splits =>
      Option.when(splits.foldLeft(0)(_ + _.percent.value) != 100) {
        "Partner time splits must be specified and sum to 100%."
      }
    )

  private def usersAndTimesErrors(users: List[ProgramUser]): List[String] =
    val partnerError = Option.unless(users.forall(_.partnerLink.exists(_.isSet))) {
      "Partnership of every investigator must be specified."
    }
    (partnerError, validatePartnerSplits) match
      case (Some(a), None)    => List(a)
      case (None, Some(b))    => List(b)
      case (Some(a), Some(b)) => List(a, b)
      case (None, None)       =>
        // Make sure every partner split requested has a matching user.
        // Only verify this if splits and users are all valid.
        partnerSplits
          .map(splits =>
            splits
              .filter(_.percent.value > 0)
              .map(ps =>
                if (ps.partner === Partner.UH && !users.hasPi(Partner.UH))
                  "Requests for time from UH must have a UH PI.".some
                else if (ps.partner =!= Partner.US && !users.hasUser(ps.partner))
                  "Non-US partner time requests must have matching collaborators.".some
                else none
              )
              .flattenOption
              .distinct
          )
          .toList
          .flatten

  private lazy val isFastTurnaround: Boolean =
    proposalType.exists {
      case ProposalType.FastTurnaround(_, _, _, _) => true
      case _                                       => false
    }

  private def attachmentErrors(attachments: AttachmentList): List[String] =
    // only validate if there is a CfP
    call.foldMap(_ =>
      val science = Option.unless(attachments.hasForType(AttachmentType.Science))(
        "Science attachment is required."
      )
      val team    = Option.unless(isFastTurnaround || attachments.hasForType(AttachmentType.Team))(
        "Team attachment is required."
      )
      List(science, team).flattenOption
    )

  private def obsErrors(
    hasDefinedObservations:   Boolean,
    hasUndefinedObservations: Boolean
  ): List[String] =
    List(
      Option.unless(hasDefinedObservations)(
        "Proposal cannot be submitted without at least one defined observation."
      ),
      Option.when(hasUndefinedObservations)(
        "Proposal cannot be submitted with undefined observations. Define them or mark them as inactive."
      )
    ).flattenOption

  def errors(
    users:                    List[ProgramUser],
    attachments:              AttachmentList,
    hasDefinedObservations:   Boolean,
    hasUndefinedObservations: Boolean
  ): List[String] = List(
    cfPError(users).toList,
    usersAndTimesErrors(users),
    attachmentErrors(attachments),
    obsErrors(hasDefinedObservations, hasUndefinedObservations)
  ).flatten

object Proposal:
  val call: Lens[Proposal, Option[CallForProposal]]        =
    Focus[Proposal](_.call)
  val category: Lens[Proposal, Option[TacCategory]]        =
    Focus[Proposal](_.category)
  val proposalType: Lens[Proposal, Option[ProposalType]]   =
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
