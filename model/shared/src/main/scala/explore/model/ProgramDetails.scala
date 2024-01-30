// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.generic.semiauto.*
import lucuma.core.model.Proposal
import lucuma.schemas.decoders.given
import lucuma.schemas.enums.ProposalStatus
import monocle.Focus
import monocle.Lens

case class ProgramDetails(
  proposal:                 Option[Proposal],
  proposalStatus:           ProposalStatus,
  pi:                       Option[ProgramUser],
  users:                    List[ProgramUserWithRole],
  programTimeEstimateRange: Option[ProgramTimeRange],
  programTimeCharge:        ProgramTime
) derives Eq:
  val allUsers = pi.fold(users)(p => ProgramUserWithRole(p, None) :: users)

object ProgramDetails:
  val proposal: Lens[ProgramDetails, Option[Proposal]]                         = Focus[ProgramDetails](_.proposal)
  val proposalStatus: Lens[ProgramDetails, ProposalStatus]                     = Focus[ProgramDetails](_.proposalStatus)
  val programTimeEstimateRange: Lens[ProgramDetails, Option[ProgramTimeRange]] =
    Focus[ProgramDetails](_.programTimeEstimateRange)

  given Decoder[ProgramDetails] = Decoder.instance(c =>
    for {
      p   <- c.get[Option[Proposal]]("proposal")
      ps  <- c.get[ProposalStatus]("proposalStatus")
      pi  <- c.get[Option[ProgramUser]]("pi")
      us  <- c.get[List[ProgramUserWithRole]]("users")
      est <- c.get[Option[ProgramTimeRange]]("timeEstimateRange")
      chg <- c.get[ProgramTime]("timeCharge")
    } yield ProgramDetails(p, ps, pi, us, est, chg)
  )
