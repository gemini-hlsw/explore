// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.ProgramType
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramReference
import lucuma.schemas.enums.ProposalStatus
import monocle.Focus
import monocle.Lens
import monocle.Optional

case class ProgramDetails(
  programType:       ProgramType,
  proposal:          Option[Proposal],
  proposalStatus:    ProposalStatus,
  pi:                Option[ProgramUser],
  users:             List[ProgramUser],
  reference:         Option[ProgramReference],
  allocations:       CategoryAllocationList,
  proprietaryMonths: NonNegInt
) derives Eq:
  val allUsers: List[ProgramUser] = pi.fold(users)(_ :: users)

object ProgramDetails:
  val proposal: Lens[ProgramDetails, Option[Proposal]]          = Focus[ProgramDetails](_.proposal)
  val proposalStatus: Lens[ProgramDetails, ProposalStatus]      = Focus[ProgramDetails](_.proposalStatus)
  val allUsers: Lens[ProgramDetails, List[ProgramUser]]         =
    Lens[ProgramDetails, List[ProgramUser]](_.allUsers)(a =>
      b => b.copy(pi = a.headOption, users = a.tail)
    )
  val reference: Lens[ProgramDetails, Option[ProgramReference]] = Focus[ProgramDetails](_.reference)
  val pi: Lens[ProgramDetails, Option[ProgramUser]]             = Focus[ProgramDetails](_.pi)
  val piPartner: Optional[ProgramDetails, Option[PartnerLink]]  =
    pi.some.andThen(ProgramUser.partnerLink)

  given Decoder[ProgramDetails] = Decoder.instance(c =>
    for {
      t  <- c.get[ProgramType]("type")
      p  <- c.get[Option[Proposal]]("proposal")
      ps <- c.get[ProposalStatus]("proposalStatus")
      pi <- c.downField("pi").as[Option[ProgramUser]]
      us <- c.get[List[ProgramUser]]("users")
      r  <-
        c.downField("reference").downField("label").success.traverse(_.as[Option[ProgramReference]])
      as <- c.downField("allocations").as[CategoryAllocationList]
      pm <- c.downField("goa").downField("proprietaryMonths").as[NonNegInt]
    } yield ProgramDetails(t, p, ps, pi, us, r.flatten, as, pm)
  )
