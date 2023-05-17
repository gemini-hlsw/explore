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
import explore.model.GroupElement
import explore.model.GroupObs
import explore.model.Grouping
import explore.model.ObsSummary
import explore.model.ProgramSummaries
import explore.model.TargetWithObs
import explore.model.reusability.given
import japgolly.scalajs.react.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.EditType.Created
import lucuma.schemas.ObservationDB.Enums.EditType.Updated
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.model.TargetWithId
import lucuma.ui.reusability.given
import monocle.Focus
import monocle.Lens
import monocle.Traversal
import queries.common.ObsQueriesGQL
import queries.common.ObsQueriesGQL.ObsEditQuery.Data.observation
import queries.common.ProgramQueriesGQL.GroupEditSubscription
import queries.common.ProgramQueriesGQL.ProgramGroupsQuery
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

    val groups: IO[List[GroupElement]] = drain[GroupElement, Unit, ProgramGroupsQuery.Data](
      _ => ProgramGroupsQuery[IO].query(props.programId),
      _.program.toList.flatMap(_.allGroupElements),
      _ => false,
      _ => ()
    )

    (targets, observations, groups).mapN(ProgramSummaries.fromLists)

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

            val obsUpdate    = ProgramSummaries.observations
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
            val groupsUpdate = ProgramSummaries.groups
              .modify(groupElements =>
                if (data.observationEdit.editType === EditType.Created)
                  groupElements :+ GroupElement(GroupObs(obsId).asLeft, none)
                else if (data.observationEdit.meta.existence === Existence.Deleted)
                  // Remove the observation from all groupElements, including from the `elements` field
                  groupElements.mapFilter(ge =>
                    val newValue: Option[Either[GroupObs, Grouping]] = ge.value.bitraverse(
                      value => if value.id === obsId then none else value.some,
                      value =>
                        value
                          .copy(elements = value.elements.filterNot(_.left.exists(_.id === obsId)))
                          .some
                    )

                    newValue.map(GroupElement.value.replace(_)(ge))
                  )
                else groupElements
              )
            obsUpdate.andThen(groupsUpdate)
          )
        )

    val updateGroups = GroupEditSubscription
      .subscribe[IO](props.programId)
      .map(_.map(data =>
        val groupId  = data.groupEdit.value.id
        val editType = data.groupEdit.editType

        // TODO: update elements (like when a group is added to another group) using parentId and parentIndex (data not available yet)
        // TODO: remove groups (data not available yet)
        // TODO: ordering using indices (data not available yet)
        editType match
          case Created =>
            ProgramSummaries.groups.modify(groupElements =>
              groupElements :+ GroupElement(data.groupEdit.value.asRight, none)
            )
          case Updated =>
            ProgramSummaries.groups
              .andThen(Traversal.fromTraverse[List, GroupElement])
              .andThen(GroupElement.grouping)
              .filter(_.id === groupId)
              .replace(data.groupEdit.value)
      ))

    // TODO Handle errors, disable transparent resubscription upon connection loss.
    (updateTargets, updateObservations, updateGroups).mapN(_.merge(_).merge(_))
