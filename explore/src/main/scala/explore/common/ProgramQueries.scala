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
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
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

  def updateObsAttachmentDescription[F[_]: Async](
    pid:  Program.Id,
    oid:  ObsAttachment.Id,
    desc: Option[NonEmptyString]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObsAttachmentMutation[F]
      .execute(
        UpdateObsAttachmentsInput(
          programId = pid,
          WHERE = WhereObsAttachment(id = WhereOrderObsAttachmentId(EQ = oid.assign).assign).assign,
          SET = ObsAttachmentPropertiesInput(description = desc.orUnassign)
        )
      )
      .void

  def updateObsAttachmentChecked[F[_]: Async](
    pid:     Program.Id,
    oid:     ObsAttachment.Id,
    checked: Boolean
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObsAttachmentMutation[F]
      .execute(
        UpdateObsAttachmentsInput(
          programId = pid,
          WHERE = WhereObsAttachment(id = WhereOrderObsAttachmentId(EQ = oid.assign).assign).assign,
          SET = ObsAttachmentPropertiesInput(checked = checked.assign)
        )
      )
      .void
