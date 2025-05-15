// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import crystal.Pot
import crystal.Throttler
import explore.givens.given
import explore.model.Attachment
import explore.model.ConfigurationRequestWithObsIds
import explore.model.Group
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ProgramDetails
import explore.model.ProgramInfo
import explore.model.ProgramSummaries
import explore.model.ProgramTimes
import explore.services.OdbApi
import explore.services.OdbGroupApi
import explore.services.OdbObservationApi
import explore.services.OdbProgramApi
import explore.utils.*
import fs2.Pipe
import fs2.Stream
import fs2.concurrent.Channel
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.WhereObservation
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import monocle.Optional
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL
import queries.common.ProgramQueriesGQL
import queries.common.ProgramQueriesGQL.GroupEditSubscription

import scala.concurrent.duration.*

case class ProgramCacheController(
  programId:           Program.Id,
  modProgramSummaries: (Pot[ProgramSummaries] => Pot[ProgramSummaries]) => IO[Unit],
  onLoad:              IO[Unit]
)(using val odbApi: OdbApi[IO], logger: Logger[IO])
// Do not remove the explicit type parameter below, it confuses the compiler.
    extends ReactFnProps[ProgramCacheController](ProgramCacheController.component)
    with CacheControllerComponent.Props[ProgramSummaries]:
  val modState                           = modProgramSummaries
  given Logger[IO]                       = logger
  val updateSignal: fs2.Stream[IO, Unit] = fs2.Stream.empty

object ProgramCacheController
    extends CacheControllerComponent[ProgramSummaries, ProgramCacheController]
    with CacheModifierUpdaters:

  private val programTimesLens: Optional[ProgramSummaries, ProgramTimes] =
    ProgramSummaries.optProgramDetails.some.andThen:
      ProgramDetails.programTimes

  private def updateProgramTimes(
    programId: Program.Id
  )(using odbApi: OdbProgramApi[IO]): IO[ProgramSummaries => ProgramSummaries] =
    odbApi
      .programTimes(programId)
      .map:
        _.fold(identity[ProgramSummaries]): times =>
          programTimesLens.replace(times)

  private def updateObservationExecution(
    obsId: Observation.Id
  )(using odbApi: OdbObservationApi[IO]): IO[ProgramSummaries => ProgramSummaries] =
    odbApi
      .observationExecution(obsId)
      .map:
        _.fold(identity[ProgramSummaries]): execution =>
          ProgramSummaries.obsExecutionPots.modify(_.updated(obsId, execution))
      .handleError: e =>
        val t = new Exception("Error getting observation execution information.", e)
        ProgramSummaries.obsExecutionPots.modify(_.setError(obsId, t))

  // TODO: Move into updateStream and use the keyedSwitchEvalMap for recursion?
  private def updateGroupTimeRange(
    groupId: Option[Group.Id]
  )(using odbApi: OdbGroupApi[IO]): IO[
    ProgramSummaries => ProgramSummaries
  ] =
    def go(
      optId: Option[Group.Id],
      fn:    ProgramSummaries => ProgramSummaries
    ): IO[ProgramSummaries => ProgramSummaries] =
      optId.fold(fn.pure[IO])(gid =>
        queryGroupTimes(gid)
          .flatMap: (parentId, newFn) =>
            go(parentId, fn.andThen(newFn))
      )
    go(groupId, identity)

  private def queryGroupTimes(groupId: Group.Id)(using
    odbApi: OdbGroupApi[IO]
  ): IO[(Option[Group.Id], ProgramSummaries => ProgramSummaries)] =
    odbApi
      .groupTimeRange(groupId)
      .map: result =>
        val fn = result
          .map(_.timeEstimateRange2)
          .fold(identity[ProgramSummaries])(timeRange =>
            ProgramSummaries.groups.modify(
              _.updatedWith(groupId)(_.map(Group.timeEstimateRange.replace(timeRange)))
            )
          )
        (result.flatMap(_.parentId), fn)

  override protected val initial: ProgramCacheController => IO[
    (ProgramSummaries, Stream[IO, ProgramSummaries => ProgramSummaries])
  ] = { props =>
    import props.given

    val optProgramDetails: IO[Option[ProgramDetails]] =
      props.odbApi.programDetails(props.programId).logTime("ProgramDetailsQuery")

    val targets: IO[List[TargetWithId]] =
      props.odbApi.allProgramTargets(props.programId).logTime("AllProgramTargets")

    val observations: IO[List[Observation]] =
      props.odbApi.allProgramObservations(props.programId).logTime("AllProgramObservations")

    val configurationRequests: IO[List[ConfigurationRequestWithObsIds]] =
      props.odbApi
        .allProgramConfigurationRequests(props.programId)
        .logTime("AllProgramConfigurationRequests")

    val groups: IO[List[Group]] =
      props.odbApi.allProgramGroups(props.programId).logTime("AllProgramGroups")

    val attachments: IO[List[Attachment]] =
      props.odbApi.allProgramAttachments(props.programId).logTime("AllProgramAttachments")

    val programs: IO[List[ProgramInfo]] =
      props.odbApi.allPrograms.logTime("AllPrograms")

    def initializeSummaries(
      observations: List[Observation],
      groups:       List[Group]
    ): IO[ProgramSummaries] =
      (optProgramDetails, targets, attachments, programs, configurationRequests).mapN:
        case (pd, ts, as, ps, crs) =>
          ProgramSummaries
            .fromLists(
              pd,
              ts,
              observations,
              groups,
              as,
              ps,
              crs
            )

    def combineTimesUpdates(
      observations: List[Observation],
      groups:       List[Group]
    ): Stream[IO, ProgramSummaries => ProgramSummaries] =
      // We want to update all the observations first, followed by the groups,
      // and then the program, because the program requires all of the observation times be calculated.
      // So, if we do it first, it could take a long time.
      Stream.emits(observations.map(_.id)).evalMap(updateObservationExecution)

    for
      (obs, groups) <- (observations, groups).parTupled
      summaries     <- initializeSummaries(obs, groups)
    yield (summaries, combineTimesUpdates(obs, groups))
  }

  override protected val updateStream
    : ProgramCacheController => Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] = {
    props =>
      // programUpdateChannel is just a sigal queue to trigger a (switchable) program times update.
      (
        Resource.make(Channel.unbounded[IO, Unit])(_.close.void),
        Resource.eval(Throttler[IO](5.seconds))
      )
        .flatMapN { (programUpdateChannel, programTimesThrottler) =>
          try {
            import props.given

            // Signal a program times update.
            val queryProgramTimes: IO[Unit] =
              programTimesThrottler.submit:
                programUpdateChannel.send(()).void

            val updateProgramTimesStream: Stream[IO, ProgramSummaries => ProgramSummaries] =
              programUpdateChannel.stream
                .switchMap(_ => Stream.eval(updateProgramTimes(props.programId)))

            def updateObservationsWorkflows(
              whereObservation: WhereObservation
            ): IO[ProgramSummaries => ProgramSummaries] =
              odbApi
                .observationWorkflows(whereObservation)
                .map:
                  _.map: m =>
                    ProgramSummaries.observations
                      .modify:
                        _.updatedWith(m.id)(_.map(Observation.workflow.replace(m.workflow)))
                  .combineAll

            // Changing the proposal's CfP can change the validations for observations,
            // eg: coordinates bounds, so we requery them all.
            // https://app.shortcut.com/lucuma/story/4412/update-warnings-without-page-reload
            val allObservationsValidationsUpdate: Pipe[
              IO,
              ProgramDetails,
              ProgramSummaries => ProgramSummaries
            ] =
              _.map(_.proposal.flatMap(_.call.map(_.id))).changes.void
                .throttle(5.seconds)
                .evalMap(_ => updateObservationsWorkflows(props.programId.toWhereObservation))

            val updateProgramDetails
              : Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programEditsSubscription(props.programId)
                .map:
                  _.broadcastThrough(
                    _.map: programDetails => // Replace program.
                      ProgramSummaries.optProgramDetails.replace(programDetails.some),
                    allObservationsValidationsUpdate
                  )

            val updateTargets: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programTargetsDeltaSubscription(props.programId)
                .map(_.map(modifyTargets(_)))

            val obsCalcObsIdPipe
              : Pipe[IO, ObsQueriesGQL.ObsCalcSubscription.Data.ObscalcUpdate, Observation.Id] =
              _.map(_.observationId)

            val obsCalcGoupIdPipe
              : Pipe[IO, ObsQueriesGQL.ObsCalcSubscription.Data.ObscalcUpdate, Group.Id] =
              _.map(_.value.flatMap(_.groupId)).filter(_.isDefined).map(_.get)

            val groupEditParentIdPipe: Pipe[
              IO,
              GroupEditSubscription.Data.GroupEdit,
              Group.Id
            ] =
              _.map(_.value.flatMap(_.parentId)).filter(_.isDefined).map(_.get)

            val obsTimesUpdates: Pipe[IO, Observation.Id, ProgramSummaries => ProgramSummaries] =
              keyedSwitchEvalMap(
                identity,
                id => updateObservationExecution(id)
              )

            // We'll request updates to all the workflows at once. Since this
            // is being updated due to the configuration request change, the workflows
            // "shouldn't" be expensive and we're unlikely to need to cancel
            // unless the same configuration request is edited so using the set
            // of ids as a key should be OK.
            val obsWorkflowUpdates: Pipe[
              IO,
              ConfigurationRequestWithObsIds,
              ProgramSummaries => ProgramSummaries
            ] = keyedSwitchEvalMap(
              _.applicableObservations.toSet,
              _.applicableObservations match
                case Nil             => IO.pure(identity)
                case nonEmptyObsList =>
                  updateObservationsWorkflows(nonEmptyObsList.toWhereObservation)
            )

            val groupTimeRangeUpdate: Pipe[IO, Group.Id, ProgramSummaries => ProgramSummaries] =
              keyedSwitchEvalMap(
                identity,
                oid => updateGroupTimeRange(oid.some)
              )

            val updateObservations: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programObservationsDeltaSubscription(props.programId)
                .map:
                  _.map(modifyObservations(_))

            val updateGroups: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programGroupsDeltaEdits(props.programId)
                .evalTap(_ => queryProgramTimes)
                .map:
                  _.broadcastThrough(
                    _.map(modifyGroups(_)),
                    groupEditParentIdPipe.andThen(groupTimeRangeUpdate)
                  )

            val updateConfigurationRequests
              : Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programConfigurationRequestsDeltaSubscription(props.programId)
                .map:
                  _.broadcastThrough(
                    _.map(modifyConfigurationRequests(_)),
                    obsWorkflowUpdates
                  )

            // Right now the programEdit subsription isn't fine grained enough to
            // differentiate what got updated, so we alway update all the attachments.
            // Hopefully this will change in the future.
            val updateAttachments: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programAttachmentsDeltaSubscription(props.programId)
                .map(_.map(modifyAttachments(_)))

            val updatePrograms: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              props.odbApi
                .programDeltaSubscription(props.programId)
                .map(_.map(modifyPrograms(_)))

            val updateObsCalc: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
              odbApi
                .obsCalcSubscription(props.programId)
                .map:
                  _.evalTap(_ => queryProgramTimes)
                    .broadcastThrough(
                      obsCalcObsIdPipe.andThen(obsTimesUpdates),
                      obsCalcGoupIdPipe.andThen(groupTimeRangeUpdate)
                    )

            // TODO Handle errors, disable transparent resubscription upon connection loss.
            List(
              updateProgramDetails,
              updateTargets,
              updateObservations,
              updateGroups,
              updateAttachments,
              updatePrograms,
              updateConfigurationRequests,
              updateObsCalc
            ).sequence.map: updateStreams =>
              (updateStreams :+ updateProgramTimesStream)
                .reduceLeft(_.merge(_))
                .reduceWithin(150.millis, _ andThen _) // Group updates within 150ms
          } catch {
            case t: Throwable =>
              println("ERROR INITIALIZING ProgramCache!")
              t.printStackTrace()
              throw t
          }
        }
  }
