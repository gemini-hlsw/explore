// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.*
import explore.cache.ProgramCache
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.given
import explore.model.ProgramSummaries
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.layout.*
import explore.model.layout.unsafe.given
import explore.model.reusability.given
import explore.model.reusability.given
import explore.observationtree.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.ui.DefaultPendingRender
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Iso
import org.scalajs.dom.window
import queries.common.ObsQueriesGQL.*
import queries.common.ProgramQueriesGQL.GroupEditSubscription
import queries.common.ProgramQueriesGQL.ProgramGroupsQuery
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.odb.ObsQueries.*
import _root_.react.common.*
import _root_.react.draggable.Axis
import _root_.react.gridlayout.*
import _root_.react.hotkeys.*
import _root_.react.hotkeys.hooks.*
import _root_.react.primereact.Button
import _root_.react.resizeDetector.*
import _root_.react.resizeDetector.hooks.*

import scala.concurrent.duration.*

object DeckShown extends NewType[Boolean]:
  inline def Shown: DeckShown  = DeckShown(true)
  inline def Hidden: DeckShown = DeckShown(false)

  extension (s: DeckShown)
    def flip: DeckShown =
      if (s.value) DeckShown.Hidden else DeckShown.Shown

type DeckShown = DeckShown.Type

case class ObsTabContents(
  vault:                    Option[UserVault],
  userId:                   Option[User.Id],
  programId:                Program.Id,
  programSummaries:         View[ProgramSummaries],
  focused:                  Focused,
  undoStacks:               View[ModelUndoStacks[IO]],
  searching:                View[Set[Target.Id]],
  expandedGroups:           View[Set[Group.Id]],
  obsAttachments:           View[ObsAttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap
) extends ReactFnProps(ObsTabContents.component):
  val focusedObs: Option[Observation.Id] = focused.obsSet.map(_.head)
  val focusedTarget: Option[Target.Id]   = focused.target
  val obsUndoStacks                      = undoStacks.zoom(ModelUndoStacks.forObsList)
  val psUndoStacks                       = undoStacks.zoom(ModelUndoStacks.forProgramSummaries)

object ObsTabContents extends TwoPanels:
  private type Props = ObsTabContents

  private val NotesMaxHeight: NonNegInt         = 3.refined
  private val TargetHeight: NonNegInt           = 18.refined
  private val TargetMinHeight: NonNegInt        = 15.refined
  private val SkyPlotHeight: NonNegInt          = 9.refined
  private val SkyPlotMinHeight: NonNegInt       = 6.refined
  private val ConstraintsMinHeight: NonNegInt   = 4.refined
  private val ConstraintsMaxHeight: NonNegInt   = 7.refined
  private val TimingWindowsMinHeight: NonNegInt = 8.refined
  private val TimingWindowsMaxHeight: NonNegInt = 12.refined
  private val ConfigurationMaxHeight: NonNegInt = 10.refined
  private val ItcMinHeight: NonNegInt           = 6.refined
  private val ItcMaxHeight: NonNegInt           = 9.refined
  private val FinderChartMinHeight: NonNegInt   = 6.refined
  private val FinderChartHeight: NonNegInt      = 9.refined
  private val DefaultWidth: NonNegInt           = 10.refined
  private val TileMinWidth: NonNegInt           = 6.refined
  private val DefaultLargeWidth: NonNegInt      = 12.refined

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(
        x = 0,
        y = 0,
        w = DefaultWidth.value,
        h = NotesMaxHeight.value,
        i = ObsTabTilesIds.NotesId.id.value,
        isResizable = false
      ),
      LayoutItem(
        x = 0,
        y = NotesMaxHeight.value,
        w = DefaultWidth.value,
        h = TargetHeight.value,
        minH = TargetMinHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.TargetId.id.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight).value,
        w = DefaultWidth.value,
        h = FinderChartHeight.value,
        minH = FinderChartMinHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.FinderChartsId.id.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight).value,
        w = DefaultWidth.value,
        h = SkyPlotHeight.value,
        minH = SkyPlotMinHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.PlotId.id.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        minH = ConstraintsMinHeight.value,
        maxH = ConstraintsMaxHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.ConstraintsId.id.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = TimingWindowsMaxHeight.value,
        minH = TimingWindowsMinHeight.value,
        maxH = TimingWindowsMaxHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.TimingWindowsId.id.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTilesIds.ConfigurationId.id.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight |+| ConfigurationMaxHeight).value,
        w = DefaultWidth.value,
        h = ItcMaxHeight.value,
        i = ObsTabTilesIds.ItcId.id.value
      )
    )
  )

  private val defaultObsLayouts: LayoutsMap =
    defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  private def renderFn(
    props:          Props,
    selectedView:   View[SelectedPanel],
    defaultLayouts: LayoutsMap,
    layouts:        View[Pot[LayoutsMap]],
    resize:         UseResizeDetectorReturn,
    deckShown:      View[DeckShown],
    ctx:            AppContext[IO]
  ): VdomNode = {
    import ctx.given

    val programSummaries: View[ProgramSummaries] = props.programSummaries
    val observations: View[ObservationList]      = programSummaries.zoom(ProgramSummaries.observations)
    val obsUndoCtx: UndoContext[ObservationList] = UndoContext(props.obsUndoStacks, observations)
    val psUndoCtx: UndoContext[ProgramSummaries] = UndoContext(props.psUndoStacks, programSummaries)
    val groupsUndoCtx                            = psUndoCtx.zoom(ProgramSummaries.groups)
    val targetsUndoCtx: UndoSetter[TargetList]   = psUndoCtx.zoom(ProgramSummaries.targets)

    def observationsTree(observations: View[ObservationList]) =
      if (deckShown.get === DeckShown.Shown) {
        ObsList(
          obsUndoCtx,
          props.programId,
          props.focusedObs,
          props.focusedTarget,
          selectedView.set(SelectedPanel.Summary),
          groupsUndoCtx.model.get,
          props.expandedGroups,
          deckShown
        ): VdomNode
      } else
        <.div(ExploreStyles.TreeToolbar)(
          Button(severity = Button.Severity.Secondary,
                 outlined = true,
                 disabled = false,
                 icon = Icons.ArrowRightFromLine,
                 onClick = deckShown.mod(_.flip)
          ).mini.compact
        )

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Observations, selectedView, ctx)

    def rightSide = (resize: UseResizeDetectorReturn) =>
      props.focusedObs.fold[VdomNode](
        Tile(
          "observations".refined,
          "Observations Summary",
          backButton.some
        )(renderInTitle =>
          ObsSummaryTable(
            props.userId,
            props.programId,
            observations.get,
            targetsUndoCtx.model.get,
            renderInTitle
          )
        // TODO: elevation view
        )
      )(obsId =>
        val indexValue = Iso.id[ObservationList].index(obsId).andThen(KeyedIndexedList.value)

        observations
          .zoom(indexValue)
          .mapValue(obsView =>
            ObsTabTiles(
              props.vault,
              props.userId,
              props.programId,
              backButton,
              // FIXME Find a better mechanism for this.
              // Something like .mapValue but for UndoContext
              obsUndoCtx.zoom(indexValue.getOption.andThen(_.get), indexValue.modify),
              psUndoCtx.zoom(ProgramSummaries.targets),
              // maybe we want constraintGroups, so we can get saner ids?
              programSummaries.get.constraintGroups.map(_._2).toSet,
              programSummaries.get.targetObservations,
              props.focusedTarget,
              props.undoStacks,
              props.searching,
              defaultLayouts,
              layouts,
              resize,
              props.obsAttachments,
              props.obsAttachmentAssignments
            ).withKey(s"${obsId.show}")
          )
      )

    makeOneOrTwoPanels(
      selectedView,
      observationsTree(observations),
      rightSide,
      RightSideCardinality.Multi,
      resize,
      ExploreStyles.ObsHiddenToolbar
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, panels) => (props.focusedObs, panels.reuseByValue)) {
        (_, _, _) => params =>
          val (focusedObs, selected) = params
          (focusedObs, selected.get) match {
            case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
            case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
            case _                            => Callback.empty
          }
      }
      // Measure its size
      .useResizeDetector()
      // Layout
      .useStateView(Pot.pending[LayoutsMap])
      // Keep a record of the initial target layout
      .useMemo(())(_ => defaultObsLayouts)
      // Restore positions from the db
      .useEffectWithDepsBy((p, _, _, _, _, _) => (p.userId, p.focusedObs))(
        (props, ctx, panels, _, layout, defaultLayout) =>
          _ => {
            import ctx.given

            GridLayouts
              .queryWithDefault[IO](
                props.userId,
                GridLayoutSection.ObservationsLayout,
                defaultLayout
              )
              .attempt
              .flatMap {
                case Right(dbLayout) =>
                  layoutPprint(dbLayout)
                  layout
                    .mod(
                      _.fold(
                        mergeMap(dbLayout, defaultLayout).ready,
                        _ => mergeMap(dbLayout, defaultLayout).ready,
                        cur => mergeMap(dbLayout, cur).ready
                      )
                    )
                    .toAsync
                case Left(_)         => IO.unit
              }
          }
      )
      .useGlobalHotkeysWithDepsBy((props, ctx, _, _, _, _) =>
        (props.focusedObs,
         props.programSummaries.get.observations.values.map(_.id).zipWithIndex.toList
        )
      ) { (props, ctx, _, _, _, _) => (obs, observationIds) =>
        import ctx.given

        val obsPos = observationIds.find(a => obs.forall(_ === a._1)).map(_._2)

        def callbacks: ShortcutCallbacks = {
          case CopyAlt1 | CopyAlt2 =>
            obs
              .map(id =>
                ExploreClipboard
                  .set(LocalClipboard.CopiedObservations(ObsIdSet.one(id)))
                  .withToast(s"Copied obs $id")
              )
              .orUnit
              .runAsync

          case PasteAlt1 | PasteAlt2 =>
            ExploreClipboard.get.flatMap {
              case LocalClipboard.CopiedObservations(idSet) =>
                val observations = props.programSummaries.zoom(ProgramSummaries.observations)
                // TODO Is this a dummy undoCtx??
                val undoCtx      = UndoContext(props.obsUndoStacks, observations)
                idSet.idSet.toList
                  .traverse(oid =>
                    cloneObs(
                      props.programId,
                      oid,
                      observationIds.length,
                      undoCtx,
                      ctx
                    )
                  )
                  .void
                  .withToast(s"Duplicating obs ${idSet.idSet.mkString_(", ")}")
              case _                                        => IO.unit
            }.runAsync

          case Down =>
            obsPos
              .filter(_ < observationIds.length)
              .flatMap { p =>
                val next = if (props.focusedObs.isEmpty) 0 else p + 1
                observationIds.lift(next).map { (obsId, _) =>
                  ctx.setPageVia(
                    AppTab.Observations,
                    props.programId,
                    Focused.singleObs(obsId),
                    SetRouteVia.HistoryPush
                  )
                }
              }
              .getOrEmpty

          case Up =>
            obsPos
              .filter(_ > 0)
              .flatMap { p =>
                observationIds.lift(p - 1).map { (obsId, _) =>
                  ctx.setPageVia(
                    AppTab.Observations,
                    props.programId,
                    Focused.singleObs(obsId),
                    SetRouteVia.HistoryPush
                  )
                }
              }
              .getOrEmpty

          case GoToSummary =>
            ctx.setPageVia(AppTab.Observations,
                           props.programId,
                           Focused.None,
                           SetRouteVia.HistoryPush
            )
        }
        UseHotkeysProps(((GoToSummary :: Up :: Down :: Nil) ::: (CopyKeys ::: PasteKeys)).toHotKeys,
                        callbacks
        )
      }
      .useStateView(DeckShown.Shown)
      .render((props, ctx, twoPanelState, resize, layouts, defaultLayout, deckShown) =>
        renderFn(props, twoPanelState, defaultLayout, layouts, resize, deckShown, ctx)
      )
