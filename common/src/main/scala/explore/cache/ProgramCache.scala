// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.given
import clue.StreamingClient
import explore.DefaultErrorPolicy
import explore.common.AsterismQueries.*
import explore.model.TargetWithObs
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.ui.reusability.given
import monocle.Focus
import monocle.Lens
import queries.common.AsterismQueriesGQL
import queries.common.AsterismQueriesGQL.AsterismGroupObsQuery
import queries.common.ObsQueriesGQL
import queries.common.ObsQueriesGQL.ObsEditQuery.Data.observation
import queries.common.TargetQueriesGQL

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ProgramCache(programId: Program.Id)(using client: StreamingClient[IO, ObservationDB]):
  given StreamingClient[IO, ObservationDB] = client

given Reusability[ProgramCache] = Reusability.by(_.programId)

object ProgramCache extends CacheComponent[ProgramCache, ProgramSummaries]:

  override protected val initial: ProgramCache => IO[ProgramSummaries] = props =>
    import props.given

    AsterismQueriesGQL
      .AsterismGroupObsQuery[IO]
      .query(props.programId)
      .map(AsterismGroupObsQuery.Data.asAsterismGroupWithObs.get)

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
