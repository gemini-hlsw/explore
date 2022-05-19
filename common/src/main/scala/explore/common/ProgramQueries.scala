// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.effect.IO
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.ReuseView
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.LiveQuery
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums._
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Lens
import queries.common.ProgramQueriesGQL._
import react.common.ReactFnProps

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
      _.programs.nodes.map(p => ProgramInfo(p.id, p.name, p.existence === Existence.Deleted))
  }

  final case class ProgramsLiveQuery(
    render:           ReuseView[List[ProgramInfo]] ==> VdomNode,
    includeDeleted:   Boolean,
    onNewData:        Reuse[List[ProgramInfo] => IO[Unit]]
  )(implicit val ctx: AppContextIO)
      extends ReactFnProps[ProgramsLiveQuery](ProgramsLiveQuery.component)

  object ProgramsLiveQuery {
    type Props = ProgramsLiveQuery

    implicit val reuseProps: Reusability[Props] = Reusability.derive

    protected val component =
      ScalaFnComponent.withReuse[Props] { props =>
        implicit val ctx = props.ctx

        LiveQuery(
          ProgramsQuery
            .query(props.includeDeleted)
            .map(ProgramsQuery.Data.asProgramInfoList)
            .flatTap(props.onNewData)
            .reRunOnResourceSignals(
              ProgramEditSubscription.subscribe[IO]()
            )
        )(props.render)
      }
  }

  def createProgram[F[_]: Async](name: Option[NonEmptyString])(implicit
    c:                                 TransactionalClient[F, ObservationDB]
  ): F[Option[ProgramInfo]] =
    CreateProgramMutation
      .execute[F](CreateProgramInput(name = name.orIgnore))
      .map(_.createProgram.map(p => ProgramInfo(p.id, p.name, false)))

  def deleteProgram[F[_]: Async](id: Program.Id)(implicit
    c:                               TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramMutation
      .execute[F](
        EditProgramInput(programId = id, existence = Existence.Deleted.assign)
      )
      .void

  def undeleteProgram[F[_]: Async](id: Program.Id)(implicit
    c:                                 TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramMutation
      .execute[F](
        EditProgramInput(programId = id, existence = Existence.Present.assign)
      )
      .void

  def updateProgramName[F[_]: Async](id: Program.Id, name: Option[NonEmptyString])(implicit
    c:                                   TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    UpdateProgramMutation
      .execute[F](
        EditProgramInput(programId = id, name = name.orUnassign)
      )
      .void
}
