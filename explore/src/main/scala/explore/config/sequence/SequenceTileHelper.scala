// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import clue.StreamingClient
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.model.AppContext
import explore.model.Observation
import explore.model.SequenceData
import japgolly.scalajs.react.*
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.reusability.given

import scala.concurrent.duration.*

trait SequenceTileHelper:

  protected case class LiveSequence(
    data:       Pot[(Option[ExecutionVisits], Option[SequenceData])],
    refreshing: Boolean
  )

  protected def useLiveSequence(
    obsId:               Observation.Id,
    targetIds:           List[Target.Id],
    customSedTimestamps: List[Timestamp]
  ): HookResult[LiveSequence] =
    for
      ctx                                     <- useContext(AppContext.ctx)
      given StreamingClient[IO, ObservationDB] = ctx.clients.odb
      visits                                  <- useEffectKeepResultOnMount(ctx.odbApi.observationVisits(obsId))
      sequenceData                            <- useEffectKeepResultOnMount(ctx.odbApi.sequenceData(obsId))
      refreshVisits                           <- useThrottledCallback(5.seconds)(visits.refresh.value.to[IO])
      refreshSequence                         <- useThrottledCallback(7.seconds)(sequenceData.refresh.to[IO])
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to observation edits
          ctx.odbApi
            .observationEditSubscription(obsId)
            .map(_.evalMap(_ => refreshSequence))
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to target edits
          targetIds
            .traverse: targetId =>
              ctx.odbApi.targetEditSubscription(targetId)
            .map(_.combineAll.evalMap(_ => refreshSequence))
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to step executed events
          ctx.odbApi
            .stepEventSubscription(obsId)
            .map:
              _.evalMap(_ => (refreshSequence >> refreshVisits).to[IO])
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to dataset edits
          ctx.odbApi
            .datasetEventSubscription(obsId)
            .map:
              _.evalMap(_ => (refreshSequence >> refreshVisits).to[IO])
      _                                       <-
        useEffectWithDeps(customSedTimestamps): _ =>
          // if the timestamp for a custom sed attachment changes, it means either a new custom sed
          // has been assigned, OR a new version of the custom sed has been uploaded. This is to
          // catch the latter case.
          refreshSequence.value
    yield LiveSequence(
      (visits.value, sequenceData.value).tupled,
      visits.isRunning || sequenceData.isRunning
    )
