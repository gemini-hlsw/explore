// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.effect.IO
import cats.syntax.all.*
import explore.model.EmptySiderealTarget
import explore.model.ProgramSummaries
import explore.services.OdbTargetApi
import explore.undo.*
import lucuma.core.model.Program
import lucuma.core.model.Target

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

  def insertTarget(
    programId:   Program.Id,
    setPage:     Option[Target.Id] => IO[Unit],
    postMessage: String => IO[Unit]
  )(using odbApi: OdbTargetApi[IO]): AsyncAction[ProgramSummaries, Target.Id, Option[Target]] =
    AsyncAction[ProgramSummaries, Target.Id, Option[Target]](
      asyncGet = odbApi
        .insertTarget(programId, EmptySiderealTarget)
        .map((_, EmptySiderealTarget.some)),
      getter = singleTargetGetter,
      setter = singleTargetSetter,
      // DB creation is performed beforehand, in order to get id
      onSet = targetId =>
        (_, otwo) =>
          otwo.fold(odbApi.deleteTargets(List(targetId), programId) >> setPage(none))(_ =>
            setPage(targetId.some)
          ),
      onRestore = targetId =>
        (_, otwo) =>
          otwo.fold(
            odbApi.deleteTargets(List(targetId), programId) >> setPage(none) >>
              postMessage(s"Re-deleted target '$targetId'")
          ) { _ =>
            odbApi.undeleteTargets(List(targetId), programId) >> setPage(targetId.some) >>
              postMessage(s"Restored target '$targetId'")
          }
    )

  def deleteTargets(
    targetIds:   List[Target.Id],
    programId:   Program.Id,
    setSummary:  IO[Unit],
    postMessage: String => IO[Unit]
  )(using odbApi: OdbTargetApi[IO]): Action[ProgramSummaries, List[Option[Target]]] =
    Action(getter = targetListGetter(targetIds), setter = targetListSetter(targetIds))(
      onSet = (_, lotwo) =>
        lotwo.sequence.fold(odbApi.deleteTargets(targetIds, programId))(_ =>
          odbApi.undeleteTargets(targetIds, programId)
        ),
      onRestore = (_, lotwo) =>
        lotwo.sequence.fold(
          odbApi.deleteTargets(targetIds, programId) >> setSummary >>
            postMessage(s"Deleted ${targetIds.length} target(s)")
        )(_ =>
          odbApi.undeleteTargets(targetIds, programId) >> setSummary >>
            postMessage(s"Restored ${targetIds.length} target(s)")
        )
    )
}
