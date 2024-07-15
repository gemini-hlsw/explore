// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.syntax.all.*
import explore.DefaultErrorPolicy
import explore.common.AsterismQueries
import explore.model.ObsIdSet
import explore.model.OnCloneParameters
import explore.model.ProgramSummaries
import explore.undo.*
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import queries.common.TargetQueriesGQL

object TargetCloneAction {
  private def getter(cloneId: Target.Id): ProgramSummaries => Option[Target] =
    _.targets.get(cloneId)

  def setter(originalId: Target.Id, clone: TargetWithId, obsIds: ObsIdSet)(
    optClone: Option[Target]
  ): ProgramSummaries => ProgramSummaries = ps =>
    // if the clone is in programs summaries, we're undoing.
    ps.targets
      .get(clone.id)
      .fold(
        ps.cloneTargetForObservations(originalId, clone, obsIds)
      )(_ => ps.unCloneTargetForObservations(originalId, clone.id, obsIds))

  def updateRemote(
    programId:    Program.Id,
    onCloneParms: OnCloneParameters
  )(using
    FetchClient[IO, ObservationDB]
  ): IO[Unit] =
    val existence = if (onCloneParms.areCreating) Existence.Present else Existence.Deleted
    TargetQueriesGQL
      .UpdateTargetsMutation[IO]
      .execute(
        UpdateTargetsInput(
          WHERE = onCloneParms.cloneId.toWhereTarget
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = existence.assign),
          includeDeleted = true.assign
        )
      ) >>
      AsterismQueries.addAndRemoveTargetsFromAsterisms(onCloneParms.obsIds.toList,
                                                       toAdd = List(onCloneParms.idToAdd),
                                                       toRemove = List(onCloneParms.idToRemove)
      )

  def cloneTarget(
    programId:  Program.Id,
    originalId: Target.Id,
    clone:      TargetWithId,
    obsIds:     ObsIdSet,
    onClone:    OnCloneParameters => Callback
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ProgramSummaries, Option[Target]] =
    Action[ProgramSummaries, Option[Target]](getter(clone.id), setter(originalId, clone, obsIds))(
      onSet = (_, _) => IO.unit, // clone is created and first `onClone` called outside of Action
      onRestore = (ps, optClone) =>
        val params = OnCloneParameters(originalId, clone.id, obsIds, optClone.isDefined)
        onClone(params).toAsync >>
          updateRemote(programId, params)
    )
}
