// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import explore.common.AsterismQueries
import explore.common.AsterismQueries.*
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import queries.common.TargetQueriesGQL
import queries.schemas.odb.ODBConversions.*

object TargetSummaryActions {
  private def singleTargetGetter(
    targetId: Target.Id
  ): AsterismGroupsWithObs => Option[TargetWithObs] = _.targetsWithObs.get(targetId)

  private def targetListGetter(
    targetIds: List[Target.Id]
  ): AsterismGroupsWithObs => List[Option[TargetWithObs]] = agwo =>
    targetIds.map(tid => singleTargetGetter(tid)(agwo))

  private def singleTargetSetter(targetId: Target.Id)(
    otwo:                                  Option[TargetWithObs]
  ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo =>
    otwo.fold(AsterismGroupsWithObs.targetsWithObs.modify(_.removed(targetId))) { tg =>
      AsterismGroupsWithObs.targetsWithObs.modify(_.updated(targetId, tg)) >>>
        AsterismGroupsWithObs.asterismGroups.modify(
          _.map { case (_, ag) =>
            if (ag.obsIds.toSortedSet.intersect(tg.obsIds).nonEmpty) ag.addTargetId(targetId)
            else ag
          }.toList.toSortedMap(_.obsIds)
        )
    }(agwo)

  private def targetListSetter(targetIds: List[Target.Id])(
    twol:                                 List[Option[TargetWithObs]]
  ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo =>
    targetIds.zip(twol).foldLeft(agwo) { case (acc, (tid, otwo)) =>
      singleTargetSetter(tid)(otwo)(acc)
    }

  private def remoteDeleteTargets(targetIds: List[Target.Id], programId: Program.Id)(using
    c:                                       TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL.DeleteTargetsMutation
      .execute[IO](
        DeleteTargetsInput(WHERE =
          targetIds.toWhereTargets
            .copy(programId = WhereOrderProgramId(programId.assign).assign)
            .assign
        )
      )
      .void

  private def remoteUndeleteTargets(targetIds: List[Target.Id], programId: Program.Id)(using
    c:                                         TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL.UndeleteTargetsMutation
      .execute[IO](
        UndeleteTargetsInput(WHERE =
          targetIds.toWhereTargets
            .copy(programId = WhereOrderProgramId(programId.assign).assign)
            .assign
        )
      )
      .void

  def insertTarget(
    targetId:    Target.Id,
    programId:   Program.Id,
    setPage:     Option[Target.Id] => IO[Unit],
    postMessage: String => IO[Unit]
  )(using
    c:           TransactionalClient[IO, ObservationDB]
  ): Action[AsterismGroupsWithObs, Option[TargetWithObs]] =
    Action[AsterismGroupsWithObs, Option[TargetWithObs]](
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
    c:           TransactionalClient[IO, ObservationDB]
  ): Action[AsterismGroupsWithObs, List[Option[TargetWithObs]]] =
    Action(getter = targetListGetter(targetIds), setter = targetListSetter(targetIds))(
      onSet = (_, lotwo) =>
        lotwo.sequence.fold(remoteDeleteTargets(targetIds, programId))(_ =>
          remoteUndeleteTargets(targetIds, programId)
        ),
      onRestore = (_, lotwo) =>
        lotwo.sequence.fold(
          remoteDeleteTargets(targetIds, programId) >> setSummary >>
            postMessage(s"Re-deleted ${targetIds.length} target(s)")
        )(_ =>
          remoteUndeleteTargets(targetIds, programId) >> setSummary >>
            postMessage(s"Restored ${targetIds.length} target(s)")
        )
    )
}
