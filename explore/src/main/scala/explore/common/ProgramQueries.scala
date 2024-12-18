// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.DefaultErrorPolicy
import explore.model.ProgramInfo
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.Attachment
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.ProgramQueriesGQL.*

object ProgramQueries:
  def createProgram[F[_]: Async](name: Option[NonEmptyString])(using
    FetchClient[F, ObservationDB]
  ): F[ProgramInfo] =
    CreateProgramMutation[F]
      .execute(
        CreateProgramInput(SET = ProgramPropertiesInput(name = name.orIgnore).assign)
      )
      .map(_.createProgram.program)

  def deleteProgram[F[_]: Async](id: Program.Id)(using
    FetchClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation[F]
      .execute(
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(existence = Existence.Deleted.assign)
        )
      )
      .void

  def undeleteProgram[F[_]: Async](id: Program.Id)(using
    FetchClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation[F]
      .execute(
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          includeDeleted = true.assign,
          SET = ProgramPropertiesInput(existence = Existence.Present.assign)
        )
      )
      .void

  def updateProgramName[F[_]: Async](id: Program.Id, name: Option[NonEmptyString])(using
    FetchClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation[F]
      .execute(
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(name = name.orUnassign)
        )
      )
      .void

  def updateAttachmentDescription[F[_]: Async](
    oid:  Attachment.Id,
    desc: Option[NonEmptyString]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateAttachmentMutation[F]
      .execute(
        UpdateAttachmentsInput(
          WHERE = WhereAttachment(id = WhereOrderAttachmentId(EQ = oid.assign).assign).assign,
          SET = AttachmentPropertiesInput(description = desc.orUnassign)
        )
      )
      .void

  def updateAttachmentChecked[F[_]: Async](
    oid:     Attachment.Id,
    checked: Boolean
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateAttachmentMutation[F]
      .execute(
        UpdateAttachmentsInput(
          WHERE = WhereAttachment(id = WhereOrderAttachmentId(EQ = oid.assign).assign).assign,
          SET = AttachmentPropertiesInput(checked = checked.assign)
        )
      )
      .void

  def updateProgramUsers[F[_]: Async](
    pid: Program.Id,
    uid: User.Id,
    set: ProgramUserPropertiesInput
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    ProgramUsersMutation[F]
      .execute(
        UpdateProgramUsersInput(
          WHERE = WhereProgramUser(
            program = WhereProgram(id = WhereOrderProgramId(EQ = pid.assign).assign).assign,
            user = WhereUser(id = WhereOrderUserId(EQ = uid.assign).assign).assign
          ).assign,
          SET = set
        )
      )
      .void

  def updateProgramPartner[F[_]: Async](
    pid: Program.Id,
    uid: User.Id,
    pl:  Option[PartnerLink]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    updateProgramUsers(pid, uid, ProgramUserPropertiesInput(partnerLink = pl.toInput.assign))

  def updateUserES[F[_]: Async](
    pid: Program.Id,
    uid: User.Id,
    es:  Option[EducationalStatus]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    updateProgramUsers(pid, uid, ProgramUserPropertiesInput(educationalStatus = es.orUnassign))

  def updateUserThesis[F[_]: Async](
    pid: Program.Id,
    uid: User.Id,
    th:  Option[Boolean]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    updateProgramUsers(pid, uid, ProgramUserPropertiesInput(thesis = th.orUnassign))

  def updateUserGender[F[_]: Async](
    pid: Program.Id,
    uid: User.Id,
    g:   Option[Gender]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    updateProgramUsers(pid, uid, ProgramUserPropertiesInput(gender = g.orUnassign))

  def updateConfigurationRequestStatus[F[_]: Async](
    rids:      List[ConfigurationRequest.Id],
    newStatus: ConfigurationRequestStatus
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateConfigurationRequestsMutation[F]
      .execute(
        UpdateConfigurationRequestsInput(
          WHERE = rids.toWhereConfigurationRequest.assign,
          SET = ConfigurationRequestProperties(
            status = newStatus.assign
          )
        )
      )
      .void
