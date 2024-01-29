// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import clue.ErrorPolicy
import clue.StreamingClient
import clue.data.syntax.*
import explore.DefaultErrorPolicy
import explore.model.GroupElement
import explore.model.ObsAttachment
import explore.model.ObsSummary
import explore.model.ProgramInfo
import explore.model.ProgramSummaries
import explore.model.ProposalAttachment
import explore.utils.reduceWithin
import japgolly.scalajs.react.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.TargetWithId
import lucuma.ui.reusability.given
import queries.common.ObsQueriesGQL
import queries.common.ProgramQueriesGQL
import queries.common.ProgramSummaryQueriesGQL
import queries.common.TargetQueriesGQL

import scala.concurrent.duration.*

case class ProgramCache(
  programId:           Program.Id,
  roleId:              Option[String], // Just to refresh if the role has changed
  setProgramSummaries: Option[ProgramSummaries] => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB])
    extends ReactFnProps[ProgramCache](ProgramCache.component)
    with CacheComponent.Props[ProgramSummaries]:
  val setState                             = setProgramSummaries
  given StreamingClient[IO, ObservationDB] = client

object ProgramCache
    extends CacheComponent[ProgramSummaries, ProgramCache]
    with CacheModifierUpdaters:
  given Reusability[ProgramCache] = Reusability.by(p => (p.programId, p.roleId))

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

  override protected val initial: ProgramCache => IO[ProgramSummaries] = { props =>
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
            .query(props.programId, offset.orUnassign)(ErrorPolicy.IgnoreOnData),
        _.observations.matches,
        _.observations.hasMore,
        _.id
      )

    val groups: IO[List[GroupElement]] =
      ProgramQueriesGQL
        .ProgramGroupsQuery[IO]
        .query(props.programId)
        .map(_.program.toList.flatMap(_.allGroupElements))

    val attachments: IO[(List[ObsAttachment], List[ProposalAttachment])] =
      ProgramSummaryQueriesGQL
        .AllProgramAttachments[IO]
        .query(props.programId)
        .map(_.program.fold(List.empty, List.empty)(p => (p.obsAttachments, p.proposalAttachments)))

    val programs: IO[List[ProgramInfo]] =
      drain[ProgramInfo, Program.Id, ProgramSummaryQueriesGQL.AllPrograms.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllPrograms[IO]
            .query(offset.orUnassign),
        _.programs.matches,
        _.programs.hasMore,
        _.id
      )

    (targets, observations, groups, attachments, programs).mapN {
      case (ts, os, gs, (oas, pas), ps) =>
        ProgramSummaries.fromLists(ts, os, gs, oas, pas, ps)
    }
  }

  override protected val updateStream: ProgramCache => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, ProgramSummaries => ProgramSummaries]
  ] = { props =>
    try {
      import props.given

      val updateTargets =
        TargetQueriesGQL.ProgramTargetsDelta
          .subscribe[IO](props.programId)
          .map(_.map(data => modifyTargets(data.targetEdit)))

      val updateObservations =
        ObsQueriesGQL.ProgramObservationsDelta
          .subscribe[IO](props.programId)(summon, ErrorPolicy.IgnoreOnData)
          .map(_.map(data => modifyObservations(data.observationEdit)))

      val updateGroups = ProgramQueriesGQL.GroupEditSubscription
        .subscribe[IO](props.programId)
        .map(_.map(data => modifyGroups(data.groupEdit)))

      // Right now the programEdit subsription isn't fine grained enough to
      // differentiate what got updated, so we alway update all the attachments.
      // Hopefully this will change in the future.
      val updateAttachments = ProgramQueriesGQL.ProgramEditAttachmentSubscription
        .subscribe[IO](props.programId)
        .map(_.map(data => modifyAttachments(data.programEdit)))

      val updatePrograms =
        ProgramQueriesGQL.ProgramInfoDelta
          .subscribe[IO]()
          .map(_.map(data => modifyPrograms(data.programEdit)))

      // TODO Handle errors, disable transparent resubscription upon connection loss.
      List(
        updateTargets,
        updateObservations,
        updateGroups,
        updateAttachments,
        updatePrograms
      ).sequence.map(
        _.reduceLeft(_.merge(_))
          .reduceWithin(150.millis, _ andThen _) // Group updates within 150ms
      )
    } catch {
      case t: Throwable =>
        println("ERROR INITIALIZING ProgramCache!")
        t.printStackTrace()
        throw t
    }
  }
