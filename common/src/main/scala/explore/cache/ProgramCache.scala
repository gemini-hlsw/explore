// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.syntax.all.given
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import scala.collection.immutable.SortedMap
import lucuma.core.model.Target
import cats.effect.kernel.Resource
import queries.common.TargetQueriesGQL
import explore.DefaultErrorPolicy
import lucuma.ui.reusability.given
import clue.StreamingClient
import lucuma.schemas.ObservationDB
import cats.Order.given
import monocle.Lens
import monocle.Focus
import explore.model.TargetWithObs
import scala.collection.immutable.SortedSet
import explore.common.AsterismQueries.*
import queries.common.AsterismQueriesGQL
import queries.common.AsterismQueriesGQL.AsterismGroupObsQuery
import queries.common.ObsQueriesGQL
import lucuma.schemas.ObservationDB.Enums.Existence

case class ProgramCache(programId: Program.Id)(using client: StreamingClient[IO, ObservationDB]):
  given StreamingClient[IO, ObservationDB] = client

given Reusability[ProgramCache] = Reusability.by(_.programId)

object ProgramCache extends CacheComponent[ProgramCache, AsterismGroupsWithObs]:

  override protected val initial: ProgramCache => IO[AsterismGroupsWithObs] = props =>
    import props.given

    AsterismQueriesGQL
      .AsterismGroupObsQuery[IO]
      .query(props.programId)
      .map(AsterismGroupObsQuery.Data.asAsterismGroupWithObs.get)

  override protected val updateStream: ProgramCache => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, AsterismGroupsWithObs => AsterismGroupsWithObs]
  ] = props =>
    import props.given

    val updateTargets =
      TargetQueriesGQL.ProgramTargetsDelta
        .subscribe[IO](props.programId)
        .map(
          _.map(data =>
            AsterismGroupsWithObs.targetsWithObs
              .modify(targets =>
                if (data.targetEdit.meta.existence === Existence.Present)
                  targets.updated(
                    data.targetEdit.value.id,
                    TargetWithObs(data.targetEdit.value.target, SortedSet.empty)
                  )
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
            AsterismGroupsWithObs.observations
              .modify(observations =>
                if (data.observationEdit.meta.existence === Existence.Present)
                  observations.updated(data.observationEdit.value.id, data.observationEdit.value)
                else
                  observations.removed(data.observationEdit.value.id)
              )
              .andThen(_.rebuildAsterismGroups)
          )
        )

    (updateTargets, updateObservations).mapN(_.merge(_))
