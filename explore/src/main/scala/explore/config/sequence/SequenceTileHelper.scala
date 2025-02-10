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
import lucuma.core.enums.ObserveClass
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
import lucuma.core.enums.StepStage

trait SequenceTileHelper:
  case class SequenceData(
    config:     InstrumentExecutionConfig,
    snPerClass: Map[ObserveClass, SignalToNoise]
  ) derives Eq

  object SequenceData:
    def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
      data.observation.flatMap: obs =>
        obs.execution.config.map: config =>
          SequenceData(
            config,
            Map.empty
            // (
            //   ObserveClass.Science     -> obs.itc.science.selected.signalToNoise,
            //   ObserveClass.Acquisition -> obs.itc.acquisition.selected.signalToNoise
            // )
          )

    given Reusability[SequenceData] = Reusability.byEq
  end SequenceData

  def useLiveSequence(
    obsId:     Observation.Id,
    targetIds: List[Target.Id]
  ): HookResult[Pot[(Option[ExecutionVisits], Option[SequenceData])]] =
    for
      ctx                                     <- useContext(AppContext.ctx)
      given StreamingClient[IO, ObservationDB] = ctx.clients.odb
      visits                                  <-
        useEffectResultOnMount:
          ObservationVisits[IO]
            .query(obsId)
            .raiseGraphQLErrors
            .map(_.observation.flatMap(_.execution))
      sequenceData                            <-
        useEffectResultOnMount:
          SequenceQuery[IO]
            .query(obsId)
            .raiseGraphQLErrors
            .map(SequenceData.fromOdbResponse)
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to observation edits
          ObsQueriesGQL.ObservationEditSubscription
            .subscribe[IO](obsId.toObservationEditInput)
            .raiseFirstNoDataError
            .ignoreGraphQLErrors
            .map(_.evalMap(_ => sequenceData.refresh.to[IO]))
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to target edits
          targetIds
            .traverse: targetId =>
              TargetQueriesGQL.TargetEditSubscription
                .subscribe[IO](targetId)
                .raiseFirstNoDataError
                .ignoreGraphQLErrors
            .map(_.combineAll.evalMap(_ => sequenceData.refresh.to[IO]))
      _                                       <-
        useEffectStreamResourceOnMount: // Subscribe to step executed events
          VisitQueriesGQL.StepSubscription
            .subscribe[IO](obsId)
            .raiseFirstNoDataError
            .ignoreGraphQLErrors
            .map:
              _.filter(_.executionEventAdded.value.stepStage === StepStage.EndStep)
                .evalMap(_ => (sequenceData.refresh >> visits.refresh).to[IO])
    yield (visits.value, sequenceData.value).tupled

  // // TODO Move this to some util package
  // import cats.effect.Async
  // import cats.effect.Sync
  // import cats.effect.Resource
  // import fs2.concurrent.Topic
  // import cats.effect.syntax.all.*
  // extension [F[_]: Async, A](stream: fs2.Stream[F, A])
  // /**
  //  * Given a `Stream`, starts it in the background and allows the creation of multiple other
  //  * `Streams` that mirror its output. Useful when we want multiple `Streams` to consume the same
  //  * output.
  //  *
  //  * When we start the same stream multiple times, we have no guarantee that all the fibers will
  //  * read all the elemnts. In particular, if the stream is backed by a queue, the fibers will
  //  * steal elements from the queue, and each element will only make it to one of the fibers.
  //  */
  // def startPublisher: Resource[F, Resource[F, fs2.Stream[F, A]]] =
  //   for
  //     t <- Resource.make(Topic[F, A])(_.close.void)
  //     _ <- stream.through(t.publish).compile.drain.background
  //   yield Resource.suspend(Sync[F].delay(t.subscribeAwaitUnbounded))
