// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.implicits.*
import clue.TransactionalClient
import clue.data.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.reusability.given
import monocle.Focus
import monocle.Lens
import queries.common.ProgramQueriesGQL.*

object ProgramQueries:
  case class ProgramInfo(id: Program.Id, name: Option[NonEmptyString], deleted: Boolean)

  object ProgramInfo:
    given Reusability[ProgramInfo] = Reusability.derive

    val id: Lens[ProgramInfo, Program.Id]               = Focus[ProgramInfo](_.id)
    val name: Lens[ProgramInfo, Option[NonEmptyString]] = Focus[ProgramInfo](_.name)
    val deleted: Lens[ProgramInfo, Boolean]             = Focus[ProgramInfo](_.deleted)

  extension (self: ProgramsQuery.Data.type)
    def asProgramInfoList: ProgramsQuery.Data => List[ProgramInfo] =
      _.programs.matches.map(p => ProgramInfo(p.id, p.name, p.existence === Existence.Deleted))

  def createProgram[F[_]: Async](name: Option[NonEmptyString])(using
    TransactionalClient[F, ObservationDB]
  ): F[ProgramInfo] =
    CreateProgramMutation
      .execute[F](
        CreateProgramInput(SET = ProgramPropertiesInput(name = name.orIgnore).assign)
      )
      .map(p => ProgramInfo(p.createProgram.program.id, p.createProgram.program.name, false))

  def deleteProgram[F[_]: Async](id: Program.Id)(using
    TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation
      .execute[F](
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(existence = Existence.Deleted.assign)
        )
      )
      .void

  def undeleteProgram[F[_]: Async](id: Program.Id)(using
    TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation
      .execute[F](
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          includeDeleted = true.assign,
          SET = ProgramPropertiesInput(existence = Existence.Present.assign)
        )
      )
      .void

  def updateProgramName[F[_]: Async](id: Program.Id, name: Option[NonEmptyString])(using
    TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation
      .execute[F](
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(name = name.orUnassign)
        )
      )
      .void
