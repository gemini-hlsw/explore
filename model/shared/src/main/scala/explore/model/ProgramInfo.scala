// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.schemas.enums.ProposalStatus
import lucuma.schemas.ObservationDB.Enums.Existence
import monocle.Focus
import monocle.Lens

case class ProgramInfo(
  id:                Program.Id,
  name:              Option[NonEmptyString],
  pi:                Option[ProgramUser],
  userRoles:         List[ProgramInfo.UserIdAndRole],
  programType:       ProgramType,
  programReference:  Option[ProgramReference],
  proposalReference: Option[ProposalReference],
  proposalStatus:    ProposalStatus,
  hasProposal:       Boolean,
  calibrationRole:   Option[CalibrationRole],
  deleted:           Boolean
) derives Eq:
  def roles(currentUserId: User.Id): List[ProgramUserRole] =
    userRoles.filter(u => u.id.contains(currentUserId)).map(_.role)

  // If the user is also support, they should be able to delete/edit
  def isReadonlyCoI(currentUserId: User.Id): Boolean =
    val rs = roles(currentUserId)
    rs.contains(ProgramUserRole.CoiRO) && !(rs.contains(ProgramUserRole.SupportPrimary) ||
      rs.contains(ProgramUserRole.SupportSecondary))

  def roleIsOK(currentUserId: User.Id, isStaff: Boolean): Boolean =
    isStaff || !isReadonlyCoI(currentUserId)

  lazy val isSystemProgram = calibrationRole.isDefined || programType === ProgramType.System

  def canDelete(currentUserId: User.Id, isStaff: Boolean): Boolean =
    val hasBeenSubmitted = proposalStatus >= ProposalStatus.Submitted
    !isSystemProgram && !hasBeenSubmitted && roleIsOK(currentUserId, isStaff)

  def canRename(currentUserId: User.Id, isStaff: Boolean): Boolean =
    !deleted && !isSystemProgram && !hasProposal && roleIsOK(currentUserId, isStaff)

object ProgramInfo:
  case class UserIdAndRole(id: Option[User.Id], role: ProgramUserRole) derives Decoder

  given Decoder[UserIdAndRole] = Decoder.instance(c =>
    for {
      id   <- c.downField("user").downField("id").success.traverse(_.as[User.Id])
      role <- c.get[ProgramUserRole]("role")
    } yield UserIdAndRole(id, role)
  )

  val id: Lens[ProgramInfo, Program.Id]               = Focus[ProgramInfo](_.id)
  val name: Lens[ProgramInfo, Option[NonEmptyString]] = Focus[ProgramInfo](_.name)
  val deleted: Lens[ProgramInfo, Boolean]             = Focus[ProgramInfo](_.deleted)

  given Decoder[ProgramInfo] = Decoder.instance(c =>
    for {
      id        <- c.get[Program.Id]("id")
      name      <- c.get[Option[NonEmptyString]]("name")
      pi        <- c.downField("pi").as[Option[ProgramUser]]
      roles     <- c.get[List[UserIdAndRole]]("users")
      pType     <- c.get[ProgramType]("type")
      progRef   <-
        c.downField("reference").downField("label").success.traverse(_.as[Option[ProgramReference]])
      propRef   <- c.downField("proposal")
                     .downField("reference")
                     .success
                     .flatMap(_.downField("label").success)
                     .traverse(_.as[Option[ProposalReference]])
      propStat  <- c.get[ProposalStatus]("proposalStatus")
      hasProp    = c.downField("proposal").downField("reference").success.isDefined
      calibRole <- c.get[Option[CalibrationRole]]("calibrationRole")
      existence <- c.get[Existence]("existence")
    } yield ProgramInfo(id,
                        name,
                        pi,
                        roles,
                        pType,
                        progRef.flatten,
                        propRef.flatten,
                        propStat,
                        hasProp,
                        calibRole,
                        existence === Existence.Deleted
    )
  )
