// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.implicits.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.*
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.AppTab
import explore.model.layout.*
import explore.model.layout.unsafe.given
import explore.model.reusability.*
import explore.model.reusability.given
import explore.observationtree.ObsList
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.scalajs.dom.window
import queries.common.ObsQueriesGQL.*
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import react.draggable.Axis
import react.gridlayout.*
import react.hotkeys.*
import react.hotkeys.*
import react.hotkeys.hooks.*
import react.hotkeys.hooks.*
import react.resizable.*
import react.resizeDetector.*
import react.resizeDetector.hooks.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes.*

import scala.concurrent.duration.*

case class ObsTabContents(
  userId:        Option[User.Id],
  programId:     Program.Id,
  focusedObs:    Option[Observation.Id],
  focusedTarget: Option[Target.Id],
  undoStacks:    View[ModelUndoStacks[IO]],
  searching:     View[Set[Target.Id]],
  hiddenColumns: View[Set[String]]
) extends ReactFnProps(ObsTabContents.component)

object ObsTabContents extends TwoResizablePanels:
  private type Props = ObsTabContents

  private val NotesMaxHeight: NonNegInt         = 3.refined
  private val TargetHeight: NonNegInt           = 18.refined
  private val TargetMinHeight: NonNegInt        = 15.refined
  private val SkyPlotHeight: NonNegInt          = 9.refined
  private val SkyPlotMinHeight: NonNegInt       = 6.refined
  private val ConstraintsMinHeight: NonNegInt   = 3.refined
  private val ConstraintsMaxHeight: NonNegInt   = 7.refined
  private val ConfigurationMaxHeight: NonNegInt = 10.refined
  private val ItcMinHeight: NonNegInt           = 6.refined
  private val ItcMaxHeight: NonNegInt           = 9.refined
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
        h = SkyPlotHeight.value,
        minH = SkyPlotMinHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.PlotId.id.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| SkyPlotHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        minH = ConstraintsMinHeight.value,
        maxH = ConstraintsMaxHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.ConstraintsId.id.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTilesIds.ConfigurationId.id.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| ConfigurationMaxHeight).value,
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
    props:              Props,
    panels:             View[TwoPanelState],
    defaultLayouts:     LayoutsMap,
    layouts:            View[Pot[LayoutsMap]],
    resize:             UseResizeDetectorReturn,
    debouncer:          Reusable[UseSingleEffect[IO]],
    ctx:                AppContext[IO]
  )(
    obsWithConstraints: View[ObsSummariesWithConstraints]
  ): VdomNode = {
    import ctx.given

    val observations     = obsWithConstraints.zoom(ObsSummariesWithConstraints.observations)
    val constraintGroups = obsWithConstraints.zoom(ObsSummariesWithConstraints.constraintGroups)

    val treeWidth    = panels.get.treeWidth.toInt
    val selectedView = panels.zoom(TwoPanelState.selected)

    def observationsTree(observations: View[ObservationList]) =
      ObsList(
        observations,
        props.programId,
        props.focusedObs,
        props.focusedTarget,
        selectedView.set(SelectedPanel.summary).reuseAlways,
        props.undoStacks.zoom(ModelUndoStacks.forObsList)
      )

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Observations, selectedView, ctx)

    val (coreWidth, coreHeight) = coreDimensions(resize, treeWidth)

    val rightSide: VdomNode =
      props.focusedObs.fold[VdomNode](
        Tile("observations".refined,
             "Observations Summary",
             backButton.some,
             key = "observationsSummary"
        )(_ =>
          <.div(
            ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
            <.div("Select or add an observation")
          )
        )
      )(obsId =>
        ObsTabTiles(
          props.userId,
          props.programId,
          obsId,
          backButton,
          constraintGroups,
          props.focusedObs,
          props.focusedTarget,
          obsWithConstraints.get.targetMap,
          props.undoStacks,
          props.searching,
          props.hiddenColumns,
          defaultLayouts,
          layouts,
          coreWidth,
          coreHeight
        ).withKey(obsId.toString)
      )

    makeOneOrTwoPanels(
      treeWidth,
      coreHeight,
      coreWidth,
      panels,
      observationsTree(observations),
      rightSide,
      RightSideCardinality.Multi,
      treeResize(props.userId,
                 panels.zoom(TwoPanelState.treeWidth),
                 ResizableSection.ObservationsTree,
                 debouncer
      )
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(TwoPanelState.initial(SelectedPanel.Uninitialized))
      .useEffectWithDepsBy((props, _, panels) =>
        (props.focusedObs, panels.zoom(TwoPanelState.selected).reuseByValue)
      ) { (_, _, _) => params =>
        val (focusedObs, selected) = params
        (focusedObs, selected.get) match {
          case (Some(_), _)                 => selected.set(SelectedPanel.editor)
          case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
          case _                            => Callback.empty
        }
      }
      // Measure its size
      .useResizeDetector()
      // Layout
      .useStateView(Pot.pending[LayoutsMap])
      // Keep a record of the initial target layouut
      .useMemo(())(_ => defaultObsLayouts)
      // Restore positions from the db
      .useEffectWithDepsBy((p, _, _, _, _, _) => (p.userId, p.focusedObs, p.focusedTarget))(
        (props, ctx, panels, _, layout, defaultLayout) =>
          _ => {
            import ctx.given

            UserGridLayoutQuery
              .queryWithDefault[IO](
                props.userId,
                GridLayoutSection.ObservationsLayout,
                ResizableSection.ObservationsTree,
                (Constants.InitialTreeWidth.toInt, defaultLayout)
              )
              .attempt
              .flatMap {
                case Right((w, dbLayout)) =>
                  (panels
                    .mod(TwoPanelState.treeWidth.replace(w.toDouble)) >>
                    layout.mod(
                      _.fold(
                        mergeMap(dbLayout, defaultLayout).ready,
                        _ => mergeMap(dbLayout, defaultLayout).ready,
                        _ => mergeMap(dbLayout, defaultLayout).ready
                      )
                    ))
                    .to[IO]
                case Left(_)              => IO.unit
              }
          }
      )
      .useSingleEffect(debounce = 1.second)
      .useStreamResourceViewWithReuseOnMountBy { (props, ctx, _, _, _, _, _) =>
        import ctx.given

        ProgramObservationsQuery
          .query(props.programId)
          .map(_.asObsSummariesWithConstraints)
          .reRunOnResourceSignals(
            ProgramObservationsEditSubscription.subscribe[IO](props.programId)
          )
      }
      .useGlobalHotkeysWithDepsBy((props, ctx, _, _, _, _, _, obsList) =>
        (props.focusedObs, obsList)
      ) { (props, ctx, _, _, _, _, _, obsList) => (obs, _) =>
        import ctx.given

        val observationIds =
          obsList.foldMap(_.value.get.observations.elements.map(_.id).zipWithIndex.toList)
        val obsPos         = observationIds.find(a => obs.forall(_ === a._1)).map(_._2)

        def callbacks: ShortcutCallbacks = {
          case CopyAlt1 | CopyAlt2 | CopyAlt3 =>
            obs
              .map(id =>
                ctx.exploreClipboard.set(LocalClipboard.CopiedObservation(id)).runAsync *> info(
                  s"Copied"
                )
              )
              .getOrEmpty
          case Down                           =>
            obsPos
              .filter(_ < observationIds.length && obsList.nonEmpty)
              .flatMap { p =>
                val next = if (props.focusedObs.isEmpty) 0 else p + 1
                observationIds.lift(next).map { (obsId, _) =>
                  ctx.setPageVia(AppTab.Observations,
                                 props.programId,
                                 Focused.singleObs(obsId),
                                 SetRouteVia.HistoryPush
                  )
                }
              }
              .getOrEmpty
          case Up                             =>
            obsPos
              .filter(_ > 0)
              .flatMap { p =>
                observationIds.lift(p - 1).map { (obsId, _) =>
                  ctx.setPageVia(AppTab.Observations,
                                 props.programId,
                                 Focused.singleObs(obsId),
                                 SetRouteVia.HistoryPush
                  )
                }
              }
              .getOrEmpty
          case GoToSummary                    =>
            ctx.setPageVia(AppTab.Observations,
                           props.programId,
                           Focused.None,
                           SetRouteVia.HistoryPush
            )
        }
        UseHotkeysProps((GoToSummary :: Up :: Down :: CopyKeys).toHotKeys, callbacks)
      }
      .render {
        (
          props,
          ctx,
          twoPanelState,
          resize,
          layouts,
          defaultLayout,
          debouncer,
          obsWithConstraints
        ) =>
          <.div(
            obsWithConstraints.render(
              renderFn(
                props,
                twoPanelState,
                defaultLayout,
                layouts,
                resize,
                debouncer,
                ctx
              ) _
            )
          ).withRef(resize.ref)
      }
