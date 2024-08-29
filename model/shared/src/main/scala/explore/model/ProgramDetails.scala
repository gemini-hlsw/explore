// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.ProgramType
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramReference
import lucuma.schemas.enums.ProposalStatus
import monocle.Focus
import monocle.Lens
import monocle.Optional

case class ProgramDetails(
  programType:    ProgramType,
  proposal:       Option[Proposal],
  proposalStatus: ProposalStatus,
  pi:             Option[ProgramUserWithRole],
  users:          List[ProgramUserWithRole],
  invitations:    List[CoIInvitation],
  reference:      Option[ProgramReference],
  allocations:    CategoryAllocationList
) derives Eq:
  val allUsers: List[ProgramUserWithRole] = pi.fold(users)(_ :: users)

object ProgramDetails:
  val proposal: Lens[ProgramDetails, Option[Proposal]]          = Focus[ProgramDetails](_.proposal)
  val proposalStatus: Lens[ProgramDetails, ProposalStatus]      = Focus[ProgramDetails](_.proposalStatus)
  val invitations: Lens[ProgramDetails, List[CoIInvitation]]    = Focus[ProgramDetails](_.invitations)
  val allUsers: Lens[ProgramDetails, List[ProgramUserWithRole]] =
    Lens[ProgramDetails, List[ProgramUserWithRole]](_.allUsers)(a =>
      b => b.copy(pi = a.headOption, users = a.tail)
    )
  val reference: Lens[ProgramDetails, Option[ProgramReference]] = Focus[ProgramDetails](_.reference)
  val pi: Lens[ProgramDetails, Option[ProgramUserWithRole]]     = Focus[ProgramDetails](_.pi)
  val piPartner: Optional[ProgramDetails, Option[PartnerLink]]  =
    pi.some.andThen(ProgramUserWithRole.partnerLink)

  given Decoder[ProgramDetails] = Decoder.instance(c =>
    for {
      t  <- c.get[ProgramType]("type")
      p  <- c.get[Option[Proposal]]("proposal")
      ps <- c.get[ProposalStatus]("proposalStatus")
      pi <- c.downField("pi").as[Option[ProgramUserWithRole]]
      us <- c.get[List[ProgramUserWithRole]]("users")
      in <- c.get[List[CoIInvitation]]("userInvitations")
      r  <-
        c.downField("reference").downField("label").success.traverse(_.as[Option[ProgramReference]])
      as <- c.downField("allocations").as[CategoryAllocationList]
    } yield ProgramDetails(t, p, ps, pi, us, in, r.flatten, as)
  )
