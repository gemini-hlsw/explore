// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import japgolly.scalajs.react._
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums._
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Lens
import queries.common.ProgramQueriesGQL._
import queries.schemas.implicits._

object ProgramQueries {
  final case class ProgramInfo(id: Program.Id, name: Option[NonEmptyString], deleted: Boolean)

  object ProgramInfo {
    implicit val reuseProgramInfo: Reusability[ProgramInfo] = Reusability.derive

    val id: Lens[ProgramInfo, Program.Id]               = Focus[ProgramInfo](_.id)
    val name: Lens[ProgramInfo, Option[NonEmptyString]] = Focus[ProgramInfo](_.name)
    val deleted: Lens[ProgramInfo, Boolean]             = Focus[ProgramInfo](_.deleted)
  }

  implicit class ProgramsQueryDataOps(val self: ProgramsQuery.Data.type) extends AnyVal {
    def asProgramInfoList: ProgramsQuery.Data => List[ProgramInfo] =
      _.programs.matches.map(p => ProgramInfo(p.id, p.name, p.existence === Existence.Deleted))
  }

  def createProgram[F[_]: Async](name: Option[NonEmptyString])(implicit
    c:                                 TransactionalClient[F, ObservationDB]
  ): F[ProgramInfo] =
    CreateProgramMutation
      .execute[F](
        CreateProgramInput(SET = ProgramPropertiesInput(name = name.orIgnore).assign)
      )
      .map(p => ProgramInfo(p.createProgram.program.id, p.createProgram.program.name, false))

  def deleteProgram[F[_]: Async](id: Program.Id)(implicit
    c:                               TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation
      .execute[F](
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(existence = Existence.Deleted.assign)
        )
      )
      .void

  def undeleteProgram[F[_]: Async](id: Program.Id)(implicit
    c:                                 TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation
      .execute[F](
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(existence = Existence.Present.assign)
        )
      )
      .void

  def updateProgramName[F[_]: Async](id: Program.Id, name: Option[NonEmptyString])(implicit
    c:                                   TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramsMutation
      .execute[F](
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(name = name.orUnassign)
        )
      )
      .void
}
