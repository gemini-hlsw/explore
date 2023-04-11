// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import explore.DefaultErrorPolicy
import explore.common.AsterismQueries
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ProgramSummaries
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.TargetQueriesGQL

object TargetAddDeleteActions {
  private def singleTargetGetter(targetId: Target.Id): ProgramSummaries => Option[Target] =
    _.targets.get(targetId)

  private def targetListGetter(
    targetIds: List[Target.Id]
  ): ProgramSummaries => List[Option[Target]] =
    ps => targetIds.map(tid => singleTargetGetter(tid)(ps))

  private def singleTargetSetter(targetId: Target.Id)(
    targetOpt: Option[Target]
  ): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.targets.modify(_.updatedWith(targetId)(_ => targetOpt))

  private def targetListSetter(targetIds: List[Target.Id])(
    targetOpts: List[Option[Target]]
  ): ProgramSummaries => ProgramSummaries = ps =>
    targetIds.zip(targetOpts).foldLeft(ps) { case (acc, (tid, targetOpt)) =>
      singleTargetSetter(tid)(targetOpt)(acc)
    }

  private def remoteDeleteTargets(targetIds: List[Target.Id], programId: Program.Id)(using
    c: FetchClient[IO, ?, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[IO]
      .execute(
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(programId = WhereOrderProgramId(programId.assign).assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Deleted.assign)
        )
      )
      .void

  private def remoteUndeleteTargets(targetIds: List[Target.Id], programId: Program.Id)(using
    c: FetchClient[IO, ?, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[IO]
      .execute(
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(programId = WhereOrderProgramId(programId.assign).assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      )
      .void

  def insertTarget(
    targetId:    Target.Id,
    programId:   Program.Id,
    setPage:     Option[Target.Id] => IO[Unit],
    postMessage: String => IO[Unit]
  )(using
    c:           FetchClient[IO, ?, ObservationDB]
  ): Action[ProgramSummaries, Option[Target]] =
    Action[ProgramSummaries, Option[Target]](
      getter = singleTargetGetter(targetId),
      setter = singleTargetSetter(targetId)
    )(
      // DB creation is performed beforehand, in order to get id
      onSet = (_, otwo) =>
        otwo.fold(remoteDeleteTargets(List(targetId), programId) >> setPage(none))(_ =>
          setPage(targetId.some)
        ),
      onRestore = (_, otwo) =>
        otwo.fold(
          remoteDeleteTargets(List(targetId), programId) >> setPage(none) >>
            postMessage(s"Re-deleted target '$targetId'")
        ) { _ =>
          remoteUndeleteTargets(List(targetId), programId) >> setPage(targetId.some) >>
            postMessage(s"Restored target '$targetId'")
        }
    )

  def deleteTargets(
    targetIds:   List[Target.Id],
    programId:   Program.Id,
    setSummary:  IO[Unit],
    postMessage: String => IO[Unit]
  )(using
    c:           FetchClient[IO, ?, ObservationDB]
  ): Action[ProgramSummaries, List[Option[Target]]] =
    Action(getter = targetListGetter(targetIds), setter = targetListSetter(targetIds))(
      onSet = (_, lotwo) =>
        lotwo.sequence.fold(remoteDeleteTargets(targetIds, programId))(_ =>
          remoteUndeleteTargets(targetIds, programId)
        ),
      onRestore = (_, lotwo) =>
        lotwo.sequence.fold(
          remoteDeleteTargets(targetIds, programId) >> setSummary >>
            postMessage(s"Deleted ${targetIds.length} target(s)")
        )(_ =>
          remoteUndeleteTargets(targetIds, programId) >> setSummary >>
            postMessage(s"Restored ${targetIds.length} target(s)")
        )
    )
}
