// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import io.circe.Decoder
import io.circe.generic.semiauto.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.ProgramReference
import lucuma.schemas.enums.ProposalStatus
import monocle.Focus
import monocle.Lens

case class ProgramDetails(
  programType:    ProgramType,
  proposal:       Option[Proposal],
  proposalStatus: ProposalStatus,
  pi:             ProgramUserWithRole,
  users:          List[ProgramUserWithRole],
  invitations:    List[CoIInvitation],
  reference:      Option[ProgramReference]
) derives Eq:
  val allUsers: NonEmptyList[ProgramUserWithRole] = NonEmptyList(pi, users)

object ProgramDetails:
  val proposal: Lens[ProgramDetails, Option[Proposal]]                  = Focus[ProgramDetails](_.proposal)
  val proposalStatus: Lens[ProgramDetails, ProposalStatus]              = Focus[ProgramDetails](_.proposalStatus)
  val invitations: Lens[ProgramDetails, List[CoIInvitation]]            = Focus[ProgramDetails](_.invitations)
  val allUsers: Lens[ProgramDetails, NonEmptyList[ProgramUserWithRole]] =
    Lens[ProgramDetails, NonEmptyList[ProgramUserWithRole]](_.allUsers)(a =>
      b => b.copy(pi = a.head, users = a.tail)
    )

  given Decoder[ProgramDetails] = Decoder.instance(c =>
    for {
      t  <- c.get[ProgramType]("type")
      p  <- c.get[Option[Proposal]]("proposal")
      ps <- c.get[ProposalStatus]("proposalStatus")
      pi <- c.get[ProgramUser]("pi")
      us <- c.get[List[ProgramUserWithRole]]("users")
      in <- c.get[List[CoIInvitation]]("userInvitations")
      r  <- c.get[Option[ProgramReference]]("reference")
    } yield ProgramDetails(t, p, ps, ProgramUserWithRole(pi, None), us, in, r)
  )
