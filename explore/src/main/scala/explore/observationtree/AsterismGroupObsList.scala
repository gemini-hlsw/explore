// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.actions.ObservationInsertAction
import explore.components.ActionButtons
import explore.components.ToolbarTooltipOptions
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.AsterismGroup
import explore.model.AsterismGroupList
import explore.model.Focused
import explore.model.LocalClipboard
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.model.ProgramSummaries
import explore.model.TargetIdSet
import explore.model.TargetList
import explore.model.TargetWithObs
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.targets.TargetAddDeleteActions
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.NewBoolean
import lucuma.react.beautifuldnd.*
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.schemas.ObservationDB
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import mouse.boolean.*
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class AsterismGroupObsList(
  programId:             Program.Id,
  focused:               Focused,
  expandedIds:           View[SortedSet[ObsIdSet]],
  undoCtx:               UndoContext[ProgramSummaries], // TODO Targets are not modified here
  selectedIdsOpt:        Option[Either[TargetIdSet, ObsIdSet]],
  clipboardContent:      LocalClipboard,
  focusTargetId:         Option[Target.Id] => Callback,
  selectSummaryTargets:  List[Target.Id] => Callback,
  modTargets:            (TargetList => TargetList) => Callback,
  copyCallback:          Callback,
  pasteCallback:         Callback,
  allocatedScienceBands: SortedSet[ScienceBand],
  readonly:              Boolean
) extends ReactFnProps[AsterismGroupObsList](AsterismGroupObsList.component)
    with ViewCommon:
  private val programSummaries: View[ProgramSummaries] = undoCtx.model

  override val obsExecutions: ObservationExecutionMap = programSummaries.get.obsExecutionPots

  private val observations: UndoSetter[ObservationList] =
    undoCtx.zoom(ProgramSummaries.observations)

  override val focusedObsSet: Option[ObsIdSet] = focused.obsSet

  private val asterismGroups: AsterismGroupList = programSummaries.get.asterismGroups

  private val selectedAsterismGroup: Option[AsterismGroup] = props.focusedObsSet
    .flatMap(idSet => asterismGroups.find { case (key, _) => idSet.subsetOf(key) })
    .map(AsterismGroup.fromTuple)

  private def targetsText(targets: SortedSet[Target.Id]): String =
    targets.size match
      case 1    => s"target ${targets.head}"
      case more => s"$more targets"

  private def observationsText(observations: ObsIdSet): String =
    observations.idSet.size match
      case 1    => s"observation ${observations.idSet.head}"
      case more => s"$more observations"

  private val selectedText: Option[String] =
    selectedIdsOpt.map(_.fold(tidSet => targetsText(tidSet.idSet.toSortedSet), observationsText))

  private val clipboardText: Option[String] =
    clipboardContent match
      case LocalClipboard.Empty                            => none
      case LocalClipboard.CopiedTargets(targets)           => targetsText(targets.idSet.toSortedSet).some
      case LocalClipboard.CopiedObservations(observations) => observationsText(observations).some

  private val selectedDisabled: Boolean =
    selectedIdsOpt.isEmpty

  private val pasteDisabled: Boolean =
    readonly ||
      clipboardContent.isEmpty ||
      clipboardContent.isTargets && selectedIdsOpt.forall(_.isLeft)

  private val pasteIntoText: Option[String] =
    selectedIdsOpt.flatMap:
      _.fold(
        targets => targetsText(targets.idSet.toSortedSet).some,
        observations =>
          props.observations.get // All focused obs have the same asterism, so we can use head
            .get(observations.idSet.head)
            .map(obs => targetsText(obs.scienceTargetIds))
      )

  private val pasteText: Option[String] =
    Option
      .unless(pasteDisabled)((clipboardText, pasteIntoText).mapN((c, s) => s"$c into $s"))
      .flatten

end AsterismGroupObsList

object AsterismGroupObsList:
  private type Props = AsterismGroupObsList

  private object AddingTargetOrObs extends NewBoolean
  private type AddingTargetOrObs = AddingTargetOrObs.Type

  private object Dragging extends NewBoolean

  private def toggleExpanded(
    obsIds:      ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]]
  ): Callback =
    expandedIds.mod { expanded =>
      expanded
        .contains_(obsIds)
        .fold(expanded - obsIds, expanded + obsIds)
    }

  private def parseDragId(dragId: String): Option[Observation.Id] =
    Observation.Id
      .parse(dragId)

  /**
   * When we're dragging an observation, we can either be dragging a single observation or a group
   * of them. If we have a selection, and the drag id is part of the selection, we drag all the
   * items in the selection. However, the user may have something selected, but be dragging
   * something that is NOT in the selection - in which case we just drag the individual item.
   */
  private def getDraggedIds(dragId: String, props: Props): Option[ObsIdSet] =
    parseDragId(dragId).map: obsId =>
      props.focusedObsSet
        .fold(ObsIdSet.one(obsId)): selectedIds =>
          if (selectedIds.contains(obsId)) selectedIds
          else ObsIdSet.one(obsId)

  private def onDragEnd(
    props: Props,
    ctx:   AppContext[IO]
  ): (DropResult, ResponderProvided) => Callback = { (result, _) =>
    import ctx.given

    val oData: Option[(ObsIdSet, SortedSet[Target.Id], ObsIdSet, ObsIdSet)] =
      for
        destination <- result.destination.toOption
        destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
        draggedIds  <- getDraggedIds(result.draggableId, props)
        destAstIds  <- props.programSummaries.get.asterismGroups.get(destIds)
        srcAg       <- props.programSummaries.get.asterismGroups.findContainingObsIds(draggedIds)
      yield (destIds, destAstIds, draggedIds, srcAg.obsIds)

    def setObsSet(obsIds: ObsIdSet): Callback =
      // if focused is empty, we're looking at the target summary table and don't want to
      // switch to editing because of drag and drop
      if (props.focused.isEmpty) Callback.empty
      else ctx.pushPage((AppTab.Targets, props.programId, Focused(obsIds.some)).some)

    oData.foldMap: (destIds, destAstIds, draggedIds, srcIds) =>
      if (destIds.intersects(draggedIds)) Callback.empty
      else
        AsterismGroupObsListActions
          .dropObservations(
            draggedIds,
            srcIds,
            destIds,
            props.expandedIds,
            setObsSet,
            props.programSummaries.get.targets
          )
          .set(props.undoCtx.zoom(ProgramSummaries.observations))(destAstIds)
  }

  private def insertSiderealTarget(
    programId:             Program.Id,
    undoCtx:               UndoContext[ProgramSummaries],
    adding:                View[AddingTargetOrObs],
    selectTargetOrSummary: Option[Target.Id] => Callback
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): IO[Unit] =
    TargetAddDeleteActions
      .insertTarget(
        programId,
        selectTargetOrSummary(_).toAsync,
        ToastCtx[IO].showToast(_)
      )(undoCtx)
      .void
      .switching(adding.async, AddingTargetOrObs(_))
      .withToastDuring("Creating target")

  private def insertObs(
    programId:          Program.Id,
    targetIds:          SortedSet[Target.Id],
    undoCtx:            UndoContext[ProgramSummaries],
    adding:             View[AddingTargetOrObs],
    expandedIds:        View[SortedSet[ObsIdSet]],
    selectObsOrSummary: Option[Observation.Id] => Callback
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): IO[Unit] =
    ObsQueries
      .createObservationWithTargets[IO](programId, targetIds)
      .flatMap: obs =>
        ObservationInsertAction
          .insert(
            obs.id,
            expandedIds,
            selectObsOrSummary(_).toAsync,
            ToastCtx[IO].showToast(_)
          )
          .set(undoCtx)(Observation.scienceTargetIds.replace(targetIds)(obs).some)
          .toAsync
      .switching(adding.async, AddingTargetOrObs(_))

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx               <- useContext(AppContext.ctx)
      dragging          <- useState(Dragging(false))
      addingTargetOrObs <- useStateView(AddingTargetOrObs(false))
      _                 <- useEffectOnMount:
                             def replacePage(focused: Focused): Callback =
                               ctx.replacePage((AppTab.Targets, props.programId, focused).some)

                             val obsMissing: Boolean =
                               props.focused.obsSet.nonEmpty && props.selectedAsterismGroup.isEmpty

                             val targetMissing: Boolean =
                               props.focused.target.fold(false)(tid =>
                                 !props.programSummaries.get.targetsWithObs.keySet.contains(tid)
                               )

                             val (newFocused, needNewPage): (Focused, Boolean) =
                               (obsMissing, targetMissing) match
                                 case (true, _) => (Focused.None, true)
                                 case (_, true) => (props.focused.withoutTarget, true)
                                 case _         => (props.focused, false)

                             val unfocus: Callback =
                               if (needNewPage) replacePage(newFocused) else Callback.empty

                             val expandSelected: Callback =
                               props.selectedAsterismGroup.foldMap(ag => props.expandedIds.mod(_ + ag.obsIds))

                             def cleanupExpandedIds: Callback =
                               props.expandedIds.mod(_.filter(props.asterismGroups.contains))

                             for
                               _ <- unfocus
                               _ <- expandSelected
                               _ <- cleanupExpandedIds
                             yield ()
    } yield
      import ctx.given

      val observations: ObservationList =
        props.programSummaries.get.observations

      val targetWithObsMap: SortedMap[Target.Id, TargetWithObs] =
        props.programSummaries.get.targetsWithObs

      val asterismGroups: List[AsterismGroup] =
        props.programSummaries.get.asterismGroups
          .map(AsterismGroup.fromTuple)
          // Remove targets whose observations are calibrations
          .filter { ag =>
            ag.obsIds.idSet.toList
              .map(observations.get)
              .flattenOption
              .forall(_.calibrationRole.isEmpty)
          }
          .toList

      // first look to see if something is focused in the tree, else see if something is focused in the summary
      val selectedTargetIds: SortedSet[Target.Id] =
        props.selectedIdsOpt.foldMap:
          _.fold(
            _.idSet.toSortedSet,
            obsIds => props.programSummaries.get.asterismGroups.get(obsIds).orEmpty
          )

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.focused.obsSet.exists(_.contains(obsId))

      def setFocused(focused: Focused): Callback =
        ctx.pushPage((AppTab.Targets, props.programId, focused).some)

      def selectObsOrSummary(oObsId: Option[Observation.Id]): Callback =
        oObsId.fold(setFocused(Focused.None))(obsId => setFocused(Focused.singleObs(obsId)))

      def renderObsClone(obsIds: ObsIdSet): Option[TagMod] =
        obsIds.single.fold {
          val list        = obsIds.toList
          val div: TagMod = <.div(
            <.div(
              list.toTagMod(using id => <.div(id.show))
            )
          )
          div.some
        }(obsId =>
          observations
            .get(obsId)
            .map(summ => props.renderObsBadge(summ, ObsBadge.Layout.TargetsTab))
        )

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          provided.dragHandleProps,
          props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          getDraggedIds(rubric.draggableId, props)
            .flatMap(renderObsClone)
            .getOrElse(<.span("ERROR"))
        )

      val handleDragEnd = onDragEnd(props, ctx)

      def handleCtrlClick(obsId: Observation.Id, groupIds: ObsIdSet) =
        props.focused.obsSet.fold(setFocused(props.focused.withSingleObs(obsId))) { selectedIds =>
          if (selectedIds.subsetOf(groupIds)) {
            if (selectedIds.contains(obsId))
              setFocused(selectedIds.removeOne(obsId).fold(Focused.None)(props.focused.withObsSet))
            else
              setFocused(props.focused.withObsSet(selectedIds.add(obsId)))
          } else Callback.empty // Not in the same group
        }

      def getAsterismGroupNames(asterismGroup: AsterismGroup): List[String] =
        asterismGroup.targetIds.toList
          .map(tid => targetWithObsMap.get(tid).map(_.target.name.value))
          .flatten

      def makeAsterismGroupName(names: List[String]): String =
        if (names.isEmpty) "<No Targets>"
        else names.mkString("; ")

      def deleteObs(asterismGroup: AsterismGroup): Observation.Id => Callback = obsId =>
        props.undoableDeleteObs(
          obsId,
          props.observations, {
            // After deletion change focus and keep expanded target
            val newObsIds = asterismGroup.obsIds - obsId
            val newFocus  =
              newObsIds.fold(Focused.None)(props.focused.withObsSet)
            val expansion =
              newObsIds.fold(Callback.empty)(a => props.expandedIds.mod(_ + a))
            expansion *> setFocused(newFocus)
          }
        )

      val deleteTargets: List[Target.Id] => Callback =
        selectedTargetsIds =>
          ConfirmDialog.confirmDialog(
            message = <.div(s"This action will delete ${props.selectedText.orEmpty}."),
            header = "Targets delete",
            acceptLabel = "Yes, delete",
            position = DialogPosition.Top,
            accept = props.modTargets(_.filter((id, _) => !selectedTargetsIds.contains(id))) >>
              TargetAddDeleteActions
                .deleteTargets(
                  selectedTargetsIds,
                  props.programId,
                  props.focusTargetId(none).toAsync,
                  ToastCtx[IO].showToast(_)
                )
                .set(props.undoCtx)(selectedTargetsIds.map(_ => none))
                .toAsync
                .runAsyncAndForget,
            acceptClass = PrimeStyles.ButtonSmall,
            rejectClass = PrimeStyles.ButtonSmall,
            icon = Icons.SkullCrossBones(^.color.red)
          )

      val deleteCallback: Callback =
        props.selectedIdsOpt
          .map:
            _.fold(
              targetIdSet => deleteTargets(targetIdSet.toList),
              obsIdSet => props.selectedAsterismGroup.map(deleteObs(_)(obsIdSet.head)).orEmpty
            )
          .orEmpty

      def renderAsterismGroup(asterismGroup: AsterismGroup, names: List[String]): VdomNode = {
        val obsIds        = asterismGroup.obsIds
        val cgObs         = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected = props.focusedObsSet.exists(_.subsetOf(obsIds))

        val icon: FontAwesomeIcon = props.expandedIds.get
          .contains_(obsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)(
            ^.cursor.pointer,
            ^.onClick ==> { (e: ReactEvent) =>
              e.stopPropagationCB >>
                toggleExpanded(obsIds, props.expandedIds)
                  .asEventDefault(e)
                  .void
            }
          )
          .withFixedWidth()

        Droppable(
          ObsIdSet.fromString.reverseGet(obsIds),
          renderClone = renderClone,
          isDropDisabled = props.readonly
        ) { case (provided, snapshot) =>
          val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
            icon,
            <.span(ExploreStyles.ObsGroupTitleWithWrap)(
              makeAsterismGroupName(names)
            ),
            <.span(ExploreStyles.ObsCount, s"${obsIds.size} Obs")
          )

          val clickFocus =
            props.focused.withObsSet(obsIds).validateOrSetTarget(asterismGroup.targetIds)

          def badgeItem(obs: Observation, idx: Int): TagMod =
            props.renderObsBadgeItem(
              ObsBadge.Layout.TargetsTab,
              selectable = true,
              highlightSelected = true,
              forceHighlight = isObsSelected(obs.id),
              linkToObsTab = false,
              onSelect = obsId =>
                setFocused(
                  props.focused
                    .withSingleObs(obsId)
                    .validateOrSetTarget(obs.scienceTargetIds)
                ),
              onDelete = deleteObs(asterismGroup)(obs.id),
              onCtrlClick = _ => handleCtrlClick(obs.id, obsIds),
              ctx = ctx
            )(obs, idx)

          <.div(
            provided.innerRef,
            provided.droppableProps,
            props.getListStyle(
              snapshot.draggingOverWith.exists(id => parseDragId(id).isDefined)
            )
          )(
            <.a(
              ExploreStyles.ObsTreeGroup |+| Option
                .when(groupSelected)(ExploreStyles.SelectedObsTreeGroup)
                .orElse(
                  Option.when(!dragging.value.value)(ExploreStyles.UnselectedObsTreeGroup)
                )
                .orEmpty
            )(
              ^.cursor.pointer,
              ^.href := ctx.pageUrl((AppTab.Targets, props.programId, clickFocus).some),
              ^.onClick ==> { (e: ReactEvent) =>
                e.preventDefaultCB *> e.stopPropagationCB *> setFocused(clickFocus)
              }
            )(
              csHeader,
              TagMod.when(props.expandedIds.get.contains(obsIds))(
                TagMod(cgObs.zipWithIndex.toTagMod(using badgeItem))
              ),
              provided.placeholder
            )
          )
        }
      }

      DragDropContext(
        onDragStart = (_: DragStart, _: ResponderProvided) =>
          dragging.setState(Dragging(true)).unless_(props.readonly),
        onDragEnd = (result, provided) =>
          (dragging.setState(Dragging(false)) >> handleDragEnd(result, provided))
            .unless_(props.readonly)
      ) {
        // make the target name sort case insensitive
        given Ordering[String] = Ordering.fromLessThan(_.toLowerCase < _.toLowerCase)
        import scala.Ordering.Implicits.seqOrdering

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            <.span(
              Button(
                label = "Obs",
                icon = Icons.New,
                tooltip = "Add a new Observation",
                tooltipOptions = ToolbarTooltipOptions.Default,
                severity = Button.Severity.Success,
                disabled = addingTargetOrObs.get.value,
                loading = addingTargetOrObs.get.value,
                onClick = insertObs(
                  props.programId,
                  selectedTargetIds,
                  props.undoCtx,
                  addingTargetOrObs,
                  props.expandedIds,
                  selectObsOrSummary
                ).runAsync
              ).compact.mini,
              Button(
                label = "Tgt",
                icon = Icons.New,
                tooltip = "Add a new Target",
                tooltipOptions = ToolbarTooltipOptions.Default,
                severity = Button.Severity.Success,
                disabled = addingTargetOrObs.get.value,
                loading = addingTargetOrObs.get.value,
                onClick = insertSiderealTarget(
                  props.programId,
                  props.undoCtx,
                  addingTargetOrObs,
                  props.focusTargetId
                ).runAsync
              ).compact.mini
            ),
            UndoButtons(props.undoCtx, size = PlSize.Mini),
            ActionButtons(
              ActionButtons.ButtonProps(
                props.copyCallback,
                disabled = props.selectedDisabled,
                tooltipExtra = props.selectedText
              ),
              ActionButtons.ButtonProps(
                props.pasteCallback,
                disabled = props.pasteDisabled,
                tooltipExtra = props.pasteText
              ),
              ActionButtons.ButtonProps(
                deleteCallback,
                disabled = props.selectedDisabled,
                tooltipExtra = props.selectedText
              )
            )
          ).unless(props.readonly),
          <.div(
            Button(
              severity = Button.Severity.Secondary,
              onClick =
                ctx.pushPage((AppTab.Targets, props.programId, props.focused.withoutObsSet).some) *>
                  props.focusTargetId(None) *>
                  props.selectSummaryTargets(props.focused.target.toList),
              clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.withClass(ExploreStyles.PaddedRightIcon),
              "Target Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              asterismGroups
                .map(ag => (ag, getAsterismGroupNames(ag)))
                .toList
                .sortBy(_._2)
                .toTagMod(using t => renderAsterismGroup(t._1, t._2))
            )
          )
        )
      }
