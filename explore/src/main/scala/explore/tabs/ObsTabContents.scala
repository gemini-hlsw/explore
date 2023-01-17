// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.layout.*
import explore.model.layout.unsafe.given
import explore.model.reusability.*
import explore.model.reusability.given
import explore.observationtree.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.undo.UndoContext
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
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.scalajs.dom.window
import queries.common.ObsQueriesGQL.*
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.odb.ObsQueries.*
import react.common.*
import react.draggable.Axis
import react.gridlayout.*
import react.hotkeys.*
import react.hotkeys.hooks.*
import react.resizeDetector.*
import react.resizeDetector.hooks.*

import scala.concurrent.duration.*

case class ObsTabContents(
  userId:     Option[User.Id],
  programId:  Program.Id,
  focused:    Focused,
  undoStacks: View[ModelUndoStacks[IO]],
  searching:  View[Set[Target.Id]]
) extends ReactFnProps(ObsTabContents.component) {
  val focusedObs: Option[Observation.Id] = focused.obsSet.map(_.head)
  val focusedTarget: Option[Target.Id]   = focused.target
  val obsListStacks                      = undoStacks.zoom(ModelUndoStacks.forObsList)
}

object ObsTabContents extends TwoPanels:
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
    selectedView:       View[SelectedPanel],
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

    def observationsTree(observations: View[ObservationList]) =
      ObsList(
        observations,
        props.programId,
        props.focusedObs,
        props.focusedTarget,
        selectedView.set(SelectedPanel.Summary),
        props.obsListStacks
      )

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Observations, selectedView, ctx)

    def rightSide = (resize: UseResizeDetectorReturn) =>
      props.focusedObs.fold[VdomNode](
        Tile(
          "observations".refined,
          "Observations Summary",
          backButton.some
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
          props.focusedTarget,
          obsWithConstraints.get.targetMap,
          props.undoStacks,
          props.searching,
          defaultLayouts,
          layouts,
          resize
        ).withKey(s"${obsId.show}")
      )

    makeOneOrTwoPanels(
      selectedView,
      observationsTree(observations),
      rightSide,
      RightSideCardinality.Multi,
      resize
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
                    .to[IO]
                case Left(_)         => IO.unit
              }
          }
      )
      .useSingleEffect(debounce = 1.second)
      .useStreamResourceViewOnMountBy { (props, ctx, _, _, _, _, _) =>
        import ctx.given

        ProgramObservationsQuery
          .query(props.programId)
          .map(_.asObsSummariesWithConstraints)
          .reRunOnResourceSignals(
            ProgramObservationsEditSubscription.subscribe[IO](props.programId)
          )
      }
      .useGlobalHotkeysWithDepsBy((props, ctx, _, _, _, _, _, obsList) =>
        (props.focusedObs,
         obsList.foldMap(_.get.observations.elements.map(_.id).zipWithIndex.toList)
        )
      ) { (props, ctx, _, _, _, _, _, obsList) => (obs, observationIds) =>
        import ctx.given

        val obsPos = observationIds.find(a => obs.forall(_ === a._1)).map(_._2)

        def callbacks: ShortcutCallbacks = {
          case CopyAlt1 | CopyAlt2 =>
            obs
              .map(id =>
                ctx.exploreClipboard
                  .set(LocalClipboard.CopiedObservations(ObsIdSet.one(id)))
                  .withToast(ctx)(s"Copied obs $id")
              )
              .orUnit
              .runAsync

          case PasteAlt1 | PasteAlt2 =>
            ctx.exploreClipboard.get.flatMap {
              case LocalClipboard.CopiedObservations(idSet) =>
                obsList.toOption.map { obsWithConstraints =>
                  val observations =
                    obsWithConstraints.zoom(ObsSummariesWithConstraints.observations)
                  val undoCtx      =
                    UndoContext(props.obsListStacks, observations)
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
                    .withToast(ctx)(s"Duplicating obs ${idSet.idSet.mkString_(", ")}")
                }.orUnit
              case _                                        => IO.unit
            }.runAsync

          case Down =>
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

          case Up =>
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
          React.Fragment(
            obsWithConstraints.render(
              renderFn(
                props,
                twoPanelState,
                defaultLayout,
                layouts,
                resize,
                debouncer,
                ctx
              ) _,
              <.span(DefaultPendingRender).withRef(resize.ref)
            )
          )
      }
