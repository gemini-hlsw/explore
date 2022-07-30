// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.implicits._
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ObsQueries._
import explore.common.UserPreferencesQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.enums.AppTab
import explore.model.layout._
import explore.model.layout.unsafe._
import explore.observationtree.ObsList
import explore.syntax.ui._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.refined._
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils._
import org.scalajs.dom.window
import queries.common.ObsQueriesGQL._
import queries.common.UserPreferencesQueriesGQL._
import react.common.ReactFnProps
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.resizeDetector._
import react.resizeDetector.hooks._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.concurrent.duration._

final case class ObsTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  focusedObs:       Option[Observation.Id],
  focusedTarget:    Option[Target.Id],
  undoStacks:       View[ModelUndoStacks[IO]],
  searching:        View[Set[Target.Id]],
  hiddenColumns:    View[Set[String]]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ObsTabContents](ObsTabContents.component)

object ObsTabTilesIds {
  val NotesId: NonEmptyString         = "notes".refined
  val TargetId: NonEmptyString        = "target".refined
  val PlotId: NonEmptyString          = "elevationPlot".refined
  val ConstraintsId: NonEmptyString   = "constraints".refined
  val ConfigurationId: NonEmptyString = "configuration".refined
}

object ObsTabContents {
  type Props = ObsTabContents

  private val NotesMaxHeight: NonNegInt         = 3.refined
  private val TargetHeight: NonNegInt           = 18.refined
  private val TargetMinHeight: NonNegInt        = 15.refined
  private val SkyPlotHeight: NonNegInt          = 9.refined
  private val SkyPlotMinHeight: NonNegInt       = 6.refined
  private val ConstraintsMinHeight: NonNegInt   = 3.refined
  private val ConstraintsMaxHeight: NonNegInt   = 7.refined
  private val ConfigurationMaxHeight: NonNegInt = 10.refined
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
        i = ObsTabTilesIds.NotesId.value,
        isResizable = false
      ),
      LayoutItem(
        x = 0,
        y = NotesMaxHeight.value,
        w = DefaultWidth.value,
        h = TargetHeight.value,
        minH = TargetMinHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.TargetId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight).value,
        w = DefaultWidth.value,
        h = SkyPlotHeight.value,
        minH = SkyPlotMinHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.PlotId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| SkyPlotHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        minH = ConstraintsMinHeight.value,
        maxH = ConstraintsMaxHeight.value,
        minW = TileMinWidth.value,
        i = ObsTabTilesIds.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTilesIds.ConfigurationId.value
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

  protected def renderFn(
    props:              Props,
    panels:             View[TwoPanelState],
    defaultLayouts:     LayoutsMap,
    layouts:            View[Pot[LayoutsMap]],
    resize:             UseResizeDetectorReturn,
    debouncer:          Reusable[UseSingleEffect[IO]]
  )(
    obsWithConstraints: View[ObsSummariesWithConstraints]
  )(implicit ctx:       AppContextIO): VdomNode = {
    val observations     = obsWithConstraints.zoom(ObsSummariesWithConstraints.observations)
    val constraintGroups = obsWithConstraints.zoom(ObsSummariesWithConstraints.constraintGroups)

    val panelsResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        panels.zoom(TwoPanelState.treeWidth).set(d.size.width.toDouble) *>
          debouncer
            .submit(
              UserWidthsCreation
                .storeWidthPreference[IO](
                  props.userId,
                  ResizableSection.ObservationsTree,
                  d.size.width
                )
            )
            .runAsync

    val treeWidth    = panels.get.treeWidth.toInt
    val selectedView = panels.zoom(TwoPanelState.selected)

    // Tree area
    def tree(observations: View[ObservationList]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableMultiPanel)(
        treeInner(observations)
      )

    def treeInner(observations: View[ObservationList]) =
      <.div(ExploreStyles.TreeBody)(
        ObsList(
          observations,
          props.programId,
          props.focusedObs,
          props.focusedTarget,
          selectedView.set(SelectedPanel.summary).reuseAlways,
          props.undoStacks.zoom(ModelUndoStacks.forObsList)
        )
      )

    val backButton: VdomNode =
      Button(
        as = <.a,
        basic = true,
        size = Mini,
        compact = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](
          ctx.pushPage(AppTab.Observations, props.programId, Focused.None) >>
            selectedView.set(SelectedPanel.tree)
        )
      )(^.href := ctx.pageUrl(AppTab.Observations, props.programId, Focused.None),
        Icons.ChevronLeft
      )

    val coreWidth =
      if (window.canFitTwoPanels) {
        resize.width.getOrElse(0)
      } else {
        resize.width.getOrElse(0) - treeWidth
      }

    val coreHeight = resize.height.getOrElse(0)

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

    if (window.canFitTwoPanels) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(observations))
          .when(panels.get.selected.leftPanelVisible),
        <.div(^.key := "obs-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(panels.get.selected.rightPanelVisible)
      )
    } else {
      <.div(
        ExploreStyles.TreeRGL,
        Resizable(
          axis = Axis.X,
          width = treeWidth.toDouble,
          height = coreHeight.toDouble,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (resize.width.getOrElse(0) / 2, 0),
          onResize = panelsResize,
          resizeHandles = List(ResizeHandleAxis.East),
          content = tree(observations)
        ),
        <.div(
          ^.key   := "obs-right-side",
          ^.width := coreWidth.px,
          ^.left  := treeWidth.px,
          ExploreStyles.SinglePanelTile
        )(
          rightSide
        )
      )
    }
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(TwoPanelState.initial(SelectedPanel.Uninitialized))
      .useEffectWithDepsBy((props, panels) =>
        (props.focusedObs, panels.zoom(TwoPanelState.selected).reuseByValue)
      ) { (_, _) => params =>
        val (focusedObs, selected) = params
        (focusedObs, selected.get) match {
          case (Some(_), _)                 => selected.set(SelectedPanel.editor)
          case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
          case _                            => Callback.empty
        }
      }
      // Measure its sive
      .useResizeDetector()
      // Layout
      .useStateView(Pot.pending[LayoutsMap])
      // Keep a record of the initial target layouut
      .useMemo(())(_ => defaultObsLayouts)
      // Restore positions from the db
      .useEffectWithDepsBy((p, _, _, _, _) => (p.userId, p.focusedObs, p.focusedTarget))(
        (props, panels, _, layout, defaultLayout) =>
          _ => {
            implicit val ctx = props.ctx

            TabGridPreferencesQuery
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
                        cur => mergeMap(dbLayout, cur).ready
                      )
                    ))
                    .to[IO]
                case Left(_)              => IO.unit
              }
          }
      )
      .useSingleEffect(debounce = 1.second)
      .useStreamResourceViewOnMountBy { (props, _, _, _, _, _) =>
        implicit val ctx = props.ctx

        ProgramObservationsQuery
          .query(props.programId)
          .map(ProgramObservationsQuery.Data.asObsSummariesWithConstraints.get)
          .reRunOnResourceSignals(
            ProgramObservationsEditSubscription.subscribe[IO](props.programId)
          )
      }
      .render {
        (
          props,
          twoPanelState,
          resize,
          layouts,
          defaultLayout,
          debouncer,
          obsWithConstraints
        ) =>
          implicit val ctx = props.ctx

          <.div(
            obsWithConstraints.render(
              renderFn(
                props,
                twoPanelState,
                defaultLayout,
                layouts,
                resize,
                debouncer
              ) _
            )
          ).withRef(resize.ref)
      }

}
