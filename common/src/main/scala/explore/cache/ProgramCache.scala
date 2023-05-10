// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import clue.data.syntax.*
import explore.DefaultErrorPolicy
import explore.common.AsterismQueries.*
import explore.model.ObsSummary
import explore.model.ProgramSummaries
import explore.model.TargetWithObs
import explore.model.reusability.given
import japgolly.scalajs.react.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.model.TargetWithId
import lucuma.ui.reusability.given
import monocle.Focus
import monocle.Lens
import queries.common.ObsQueriesGQL
import queries.common.ObsQueriesGQL.ObsEditQuery.Data.observation
import queries.common.ProgramSummaryQueriesGQL
import queries.common.TargetQueriesGQL
import react.common.ReactFnProps

case class ProgramCache(
  programId:           Program.Id,
  setProgramSummaries: Option[ProgramSummaries] => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB])
    extends ReactFnProps[ProgramCache](ProgramCache.component)
    with CacheComponent.Props[ProgramSummaries]:
  val setState                             = setProgramSummaries
  given StreamingClient[IO, ObservationDB] = client

object ProgramCache extends CacheComponent[ProgramSummaries, ProgramCache]:
  given Reusability[ProgramCache] = Reusability.by(_.programId)

  private def drain[A, Id, R](
    fetch:      Option[Id] => IO[R],
    getList:    R => List[A],
    getHasMore: R => Boolean,
    getId:      A => Id
  ): IO[List[A]] = {
    def go(id: Option[Id], accum: List[A]): IO[List[A]] =
      fetch(id).flatMap(result =>
        val list = getList(result)
        if (getHasMore(result)) go(list.lastOption.map(getId), list)
        // Fetching with offset includes the offset, so .dropRight(1) ensures we don't include it twice.
        else (accum.dropRight(1) ++ list).pure[IO]
      )

    go(none, List.empty)
  }

  override protected val initial: ProgramCache => IO[ProgramSummaries] = props =>
    import props.given

    val targets: IO[List[TargetWithId]] =
      drain[TargetWithId, Target.Id, ProgramSummaryQueriesGQL.AllProgramTargets.Data](
        offset =>
          ProgramSummaryQueriesGQL.AllProgramTargets[IO].query(props.programId, offset.orUnassign),
        _.targets.matches,
        _.targets.hasMore,
        _.id
      )

    val observations: IO[List[ObsSummary]] =
      drain[ObsSummary, Observation.Id, ProgramSummaryQueriesGQL.AllProgramObservations.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllProgramObservations[IO]
            .query(props.programId, offset.orUnassign),
        _.observations.matches,
        _.observations.hasMore,
        _.id
      )

    (targets, observations).mapN(ProgramSummaries.fromLists)

  override protected val updateStream: ProgramCache => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, ProgramSummaries => ProgramSummaries]
  ] = props =>
    import props.given

    val updateTargets =
      TargetQueriesGQL.ProgramTargetsDelta
        .subscribe[IO](props.programId)
        .map(
          _.map(data =>
            ProgramSummaries.targets
              .modify(targets =>
                if (data.targetEdit.meta.existence === Existence.Present)
                  targets.updated(data.targetEdit.value.id, data.targetEdit.value.target)
                else
                  targets.removed(data.targetEdit.value.id)
              )
          )
        )

    val updateObservations =
      ObsQueriesGQL.ProgramObservationsDelta
        .subscribe[IO](props.programId)
        .map(
          _.map(data =>
            val obsId = data.observationEdit.value.id
            ProgramSummaries.observations
              .modify(observations =>
                if (data.observationEdit.meta.existence === Existence.Present)
                  observations.inserted(
                    obsId,
                    data.observationEdit.value,
                    observations.getIndex(obsId).getOrElse(observations.length)
                  )
                else
                  observations.removed(obsId)
              )
          )
        )

    // TODO Handle errors, disable transparent resubscription upon connection loss.
    (updateTargets, updateObservations).mapN(_.merge(_))
