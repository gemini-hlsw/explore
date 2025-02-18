// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import clue.StreamingClient
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.model.AppContext
import explore.model.Observation
import japgolly.scalajs.react.*
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.odb.SequenceQueriesGQL.*
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL
import queries.common.VisitQueriesGQL
import queries.common.VisitQueriesGQL.*

import scala.concurrent.duration.*

trait SequenceTileHelper:
  protected case class SequenceData(
    config:     InstrumentExecutionConfig,
    snPerClass: Map[SequenceType, SignalToNoise]
  ) derives Eq

  protected object SequenceData:
    def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
      data.observation.flatMap: obs =>
        obs.execution.config.map: config =>
          SequenceData(
            config,
            Map.empty
            // (
            //   SequenceType.Science     -> obs.itc.science.selected.signalToNoise,
            //   SequenceType.Acquisition -> obs.itc.acquisition.selected.signalToNoise
            // )
          )

    given Reusability[SequenceData] = Reusability.byEq
  end SequenceData

  protected case class LiveSequence(
    data:       Pot[(Option[ExecutionVisits], Option[SequenceData])],
    refreshing: Boolean
  )

  protected def useLiveSequence(
    obsId:     Observation.Id,
    targetIds: List[Target.Id]
  ): HookResult[LiveSequence] =
    for
      ctx                                     <- useContext(AppContext.ctx)
      given StreamingClient[IO, ObservationDB] = ctx.clients.odb
      visits                                  <-
        useEffectKeepResultOnMount:
          ObservationVisits[IO]
            .query(obsId)
            .raiseGraphQLErrors
            .map(_.observation.flatMap(_.execution))
      sequenceData                            <-
        useEffectKeepResultOnMount:
          SequenceQuery[IO]
            .query(obsId)
            .raiseGraphQLErrors
            .map(SequenceData.fromOdbResponse)
      refreshVisits                           <-
        useThrottledCallback(5.seconds)(visits.refresh.to[IO])
      refreshSequence                         <-
        useThrottledCallback(7.seconds)(sequenceData.refresh.to[IO])
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to observation edits
          ObsQueriesGQL.ObservationEditSubscription
            .subscribe[IO](obsId.toObservationEditInput)
            .raiseFirstNoDataError
            .ignoreGraphQLErrors
            .map(_.evalMap(_ => refreshSequence))
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to target edits
          targetIds
            .traverse: targetId =>
              TargetQueriesGQL.TargetEditSubscription
                .subscribe[IO](targetId)
                .raiseFirstNoDataError
                .ignoreGraphQLErrors
            .map(_.combineAll.evalMap(_ => refreshSequence))
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to step executed events
          VisitQueriesGQL.StepSubscription
            .subscribe[IO](obsId)
            .raiseFirstNoDataError
            .ignoreGraphQLErrors
            .map:
              _.filter: data =>
                List(StepStage.StartStep, StepStage.EndStep)
                  .contains_(data.executionEventAdded.value.stepStage)
              .evalMap(_ => (refreshSequence >> refreshVisits).to[IO])
    yield LiveSequence(
      (visits.value, sequenceData.value).tupled,
      visits.isRunning || sequenceData.isRunning
    )
