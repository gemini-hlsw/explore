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
import explore.common.ObsQueries
import explore.common.ObsQueries._
import explore.common.UserPreferencesQueries._
import explore.components.Tile
import explore.components.TileController
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.display._
import explore.model.enum.AppTab
import explore.model.layout._
import explore.model.layout.unsafe._
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.optics._
import explore.syntax.ui._
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.all._
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability._
import lucuma.ui.utils._
import org.scalajs.dom.window
import queries.common.ObsQueriesGQL._
import queries.common.UserPreferencesQueriesGQL._
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.resizeDetector._
import react.resizeDetector.hooks._
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.sizes._

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

final case class ObsTabContents(
  userId:           ReuseViewOpt[User.Id],
  focusedObs:       Option[Observation.Id],
  focusedTarget:    Option[Target.Id],
  undoStacks:       ReuseView[ModelUndoStacks[IO]],
  searching:        ReuseView[Set[Target.Id]],
  hiddenColumns:    ReuseView[Set[String]]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ObsTabContents](ObsTabContents.component)

object ObsTabTiles {
  val NotesId: NonEmptyString         = "notes"
  val TargetId: NonEmptyString        = "target"
  val PlotId: NonEmptyString          = "elevationPlot"
  val ConstraintsId: NonEmptyString   = "constraints"
  val ConfigurationId: NonEmptyString = "configuration"
}

object ObsTabContents {
  private val NotesMaxHeight: NonNegInt         = 3
  private val TargetMinHeight: NonNegInt        = 12
  private val ElevationPlotMinHeight: NonNegInt = 8
  private val ConstraintsMaxHeight: NonNegInt   = 6
  private val ConfigurationMaxHeight: NonNegInt = 10
  private val DefaultWidth: NonNegInt           = 12

  private val layoutLarge: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = NotesMaxHeight.value,
                 i = ObsTabTiles.NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultWidth.value,
                 h = TargetMinHeight.value,
                 i = ObsTabTiles.TargetId.value
      ),
      LayoutItem(x = 0,
                 y = (NotesMaxHeight |+| TargetMinHeight).value,
                 w = DefaultWidth.value,
                 h = ElevationPlotMinHeight.value,
                 i = ObsTabTiles.PlotId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetMinHeight |+| ElevationPlotMinHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetMinHeight |+| ElevationPlotMinHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTiles.ConfigurationId.value
      )
    )
  )

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = NotesMaxHeight.value,
                 i = ObsTabTiles.NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultWidth.value,
                 h = TargetMinHeight.value,
                 i = ObsTabTiles.TargetId.value
      ),
      LayoutItem(x = 0,
                 y = (NotesMaxHeight |+| TargetMinHeight).value,
                 w = DefaultWidth.value,
                 h = ElevationPlotMinHeight.value,
                 i = ObsTabTiles.PlotId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetMinHeight |+| ElevationPlotMinHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetMinHeight |+| ElevationPlotMinHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTiles.ConfigurationId.value
      )
    )
  )

  private val layoutSmall: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = NotesMaxHeight.value,
                 i = ObsTabTiles.NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultWidth.value,
                 h = TargetMinHeight.value,
                 i = ObsTabTiles.TargetId.value
      ),
      LayoutItem(x = 0,
                 y = (NotesMaxHeight |+| TargetMinHeight).value,
                 w = DefaultWidth.value,
                 h = ElevationPlotMinHeight.value,
                 i = ObsTabTiles.PlotId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetMinHeight |+| ElevationPlotMinHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetMinHeight |+| ElevationPlotMinHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTiles.ConfigurationId.value
      )
    )
  )

  private val defaultLayout: LayoutsMap =
    defineStdLayouts(
      Map(
        (BreakpointName.lg, layoutLarge),
        (BreakpointName.md, layoutMedium),
        (BreakpointName.sm, layoutSmall)
      )
    )

  type Props = ObsTabContents
  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val selectedLens = TwoPanelState.selected[Observation.Id]

  def makeConstraintsSelector(
    constraintGroups: ReuseView[ConstraintsList],
    obsView:          Pot[ReuseView[ObservationData]]
  )(implicit ctx:     AppContextIO): VdomNode =
    potRender[ReuseView[ObservationData]] {
      Reuse.always { vod =>
        val cgOpt: Option[ConstraintGroup] =
          constraintGroups.get.find(_._1.contains(vod.get.id)).map(_._2)

        Select(
          value = cgOpt.map(cg => ObsIdSet.fromString.reverseGet(cg.obsIds)).orEmpty,
          onChange = (p: Dropdown.DropdownProps) => {
            val newCgOpt =
              ObsIdSet.fromString
                .getOption(p.value.toString)
                .flatMap(ids => constraintGroups.get.get(ids))
            newCgOpt.map { cg =>
              vod.zoom(ObservationData.constraintSet).set(cg.constraintSet) >>
                ObsQueries
                  .updateObservationConstraintSet[IO](List(vod.get.id), cg.constraintSet)
                  .runAsyncAndForget
            }.getOrEmpty
          },
          options = constraintGroups.get
            .map(kv =>
              new SelectItem(value = ObsIdSet.fromString.reverseGet(kv._1),
                             text = kv._2.constraintSet.shortName
              )
            )
            .toList
        )
      }
    }(obsView)

  def otherObsCount(
    targetObsMap: SortedMap[Target.Id, Set[Observation.Id]],
    obsId:        Observation.Id,
    targetId:     Target.Id
  ): Int =
    targetObsMap.get(targetId).fold(0)(obsIds => (obsIds - obsId).size)

  // TODO Use this method
  // def readTargetPreferences(p: Props, targetId: Target.Id, state: View[State])(implicit ctx: AppContextIO): Callback =
  //   p.userId.get.map { uid =>
  //     UserTargetPreferencesQuery
  //       .queryWithDefault[IO](uid, targetId, Constants.InitialFov)
  //       .flatMap(v => state.modStateIn[IO](State.fovAngle.replace(v)))
  //       .runAsyncAndForget
  //   }.getOrEmpty

  protected def renderFn(
    props:              Props,
    panels:             ReuseView[TwoPanelState[Observation.Id]],
    options:            ReuseView[TargetVisualOptions],
    layouts:            ReuseView[LayoutsMap],
    resize:             UseResizeDetectorReturn,
    debouncer:          Reusable[UseSingleEffect[IO]],
    obsWithConstraints: ReuseView[ObsSummariesWithConstraints]
  )(implicit ctx:       AppContextIO): VdomNode = {
    val observations     = obsWithConstraints.zoom(ObsSummariesWithConstraints.observations)
    val constraintGroups = obsWithConstraints.zoom(ObsSummariesWithConstraints.constraintGroups)

    val panelsResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        panels.zoom(TwoPanelState.treeWidth[Observation.Id]).set(d.size.width.toDouble) *>
          debouncer
            .submit(
              UserWidthsCreation
                .storeWidthPreference[IO](props.userId.get,
                                          ResizableSection.ObservationsTree,
                                          d.size.width
                )
            )
            .runAsyncAndForget

    val treeWidth    = panels.get.treeWidth.toInt
    val selectedView = panels.zoom(selectedLens)

    // Tree area
    def tree(observations: ReuseView[ObservationList]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableMultiPanel)(
        treeInner(observations)
      )

    def treeInner(observations: ReuseView[ObservationList]) =
      <.div(ExploreStyles.TreeBody)(
        ObsList(
          observations,
          props.focusedObs,
          props.focusedTarget,
          selectedView.set(SelectedPanel.summary).reuseAlways,
          props.undoStacks.zoom(ModelUndoStacks.forObsList)
        )
      )

    val obsIdOpt: Option[Observation.Id] = panels.get.selected.optValue

    val obsSummaryOpt: Option[ObsSummaryWithTargetsAndConstraints] =
      obsIdOpt.flatMap(observations.get.getElement)

    val targetCoords = (obsSummaryOpt, props.focusedTarget)
      .mapN((summ, id) => summ.targets.find(_.id === id).flatMap(_.coords))
      .flatten

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        basic = true,
        size = Mini,
        compact = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](
          ctx.pushPage(AppTab.Observations, none, none) >> selectedView.set(SelectedPanel.tree)
        )
      )(^.href := ctx.pageUrl(AppTab.Observations, none, none), Icons.ChevronLeft)
    )

    val coreWidth  =
      if (window.canFitTwoPanels) {
        resize.width.getOrElse(0)
      } else {
        resize.width.getOrElse(0) - treeWidth
      }
    val coreHeight = resize.height.getOrElse(0)

    val notesTile =
      Tile(
        ObsTabTiles.NotesId,
        s"Note for Observer",
        backButton.some,
        canMinimize = true
      )(
        Reuse.always(_ =>
          <.div(
            ExploreStyles.NotesWrapper,
            <.div(
              ExploreStyles.ObserverNotes,
              "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
            )
          ).reuseAlways
        )
      )

    def rightSideRGL(obsId: Observation.Id) =
      LiveQueryRenderMod[ObservationDB, ObsEditQuery.Data, Pot[ObservationData]](
        ObsEditQuery.query(obsId).reuseAlways,
        (ObsEditQuery.Data.observation.get _)
          .andThen(_.toRight(new Exception(s"Observation [$obsId] not found")).toTry.toPot)
          .reuseAlways,
        List(ObservationEditSubscription.subscribe[IO](obsId)).reuseAlways
      )(
        // TODO Better declare reusability. We might need higher arities than defined now.
        // Something like this is what we strive for:
        // (props.userId.get, coreWidth, defaultLayout, layouts, notesTile, targetId, props.searching, state.zoom(State.options), obsPot).curryReusing.in(
        // (_: Pot[View[Pot[ObservationData]]]).curryReusing.in
        Reuse.by(
          (props.userId.get,
           coreWidth,
           defaultLayout,
           layouts,
           notesTile,
           targetCoords,
           props.undoStacks,
           props.searching,
           options,
           constraintGroups
          )
        ) { (obsViewPot: Pot[ReuseView[Pot[ObservationData]]]) =>
          val obsView: Pot[ReuseView[ObservationData]] =
            obsViewPot.flatMap(view =>
              view.get.map(obs => view.map(_.zoom(_ => obs)(mod => _.map(mod))))
            )

          val constraintsSelector =
            Reuse.by((constraintGroups, obsView.map(_.get.constraintSet)))(
              makeConstraintsSelector(constraintGroups, obsView)
            )

          val skyPlotTile =
            ElevationPlotTile.elevationPlotTile(coreWidth, coreHeight, targetCoords)

          def setCurrentTarget(
            oid: Option[Observation.Id],
            tid: Option[Target.Id],
            via: SetRouteVia
          ): Callback =
            ctx.setPageVia(AppTab.Observations, oid.map(ObsIdSet.one(_)), tid, via)

          val targetTile = AsterismEditorTile.asterismEditorTile(
            props.userId.get,
            ObsIdSet.one(obsId),
            obsView.map(
              _.map(
                _.zoom(
                  ObservationData.targetEnvironment.andThen(
                    ObservationData.TargetEnvironment.asterism
                  )
                )
              )
            ),
            props.focusedTarget,
            Reuse(setCurrentTarget _)(props.focusedObs),
            Reuse.currying(obsWithConstraints.get.targetMap, obsId).in(otherObsCount _),
            props.undoStacks.zoom(ModelUndoStacks.forSiderealTarget),
            props.searching,
            options,
            "Targets",
            none,
            props.hiddenColumns,
            coreWidth,
            coreHeight
          )

          // The ExploreStyles.ConstraintsTile css adds a z-index to the constraints tile react-grid wrapper
          // so that the constraints selector dropdown always appears in front of any other tiles. If more
          // than one tile ends up having dropdowns in the tile header, we'll need something more complex such
          // as changing the css classes on the various tiles when the dropdown is clicked to control z-index.
          val constraintsTile = ConstraintsTile
            .constraintsTile(
              obsId,
              obsView.map(_.zoom(ObservationData.constraintSet)),
              props.undoStacks
                .zoom(ModelUndoStacks.forConstraintGroup[IO])
                .zoom(atMapWithDefault(ObsIdSet.one(obsId), UndoStacks.empty)),
              control = constraintsSelector.some,
              clazz = ExploreStyles.ConstraintsTile.some
            )

          val configurationTile = ConfigurationTile.configurationTile(
            obsId,
            obsView.map(_.zoom(scienceDataForObs)),
            props.undoStacks
              .zoom(ModelUndoStacks.forScienceData[IO])
              .zoom(atMapWithDefault(obsId, UndoStacks.empty))
          )

          TileController(
            props.userId.get,
            coreWidth,
            defaultLayout,
            layouts,
            List(
              targetTile,
              notesTile,
              skyPlotTile,
              constraintsTile,
              configurationTile
            ),
            GridLayoutSection.ObservationsLayout,
            clazz = ExploreStyles.ObservationTiles.some
          ): VdomNode
        }
      ).withKey(obsId.toString)

    val rightSide =
      obsIdOpt.fold[VdomNode](
        Tile("observations", "Observations Summary", backButton.some, key = "observationsSummary")(
          Reuse.by(obsWithConstraints)((_: Tile.RenderInTitle) =>
            <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                  <.div("Select or add an observation")
            )
          )
        )
      )(obsId => <.div(ExploreStyles.TreeRGLWrapper, rightSideRGL(obsId)))

    val body = if (window.canFitTwoPanels) {
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
          height = resize.height.getOrElse[Int](0).toDouble,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (resize.width.getOrElse(0) / 2, 0),
          onResize = panelsResize,
          resizeHandles = List(ResizeHandleAxis.East),
          content = tree(observations)
        ),
        <.div(^.key   := "obs-right-side",
              ^.width := coreWidth.px,
              ^.left  := treeWidth.px,
              ExploreStyles.SinglePanelTile
        )(
          rightSide
        )
      )
    }
    body.withRef(resize.ref)
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse(TwoPanelState.initial[Observation.Id](SelectedPanel.Uninitialized))
      .useEffectWithDepsBy((props, panels) =>
        (props.focusedObs, panels.zoom(selectedLens).value.reuseByValue)
      ) { (_, _) => params =>
        val (focusedObs, selected) = params
        (focusedObs, selected.get) match {
          case (Some(obsId), _)                => selected.set(SelectedPanel.editor(obsId))
          case (None, SelectedPanel.Editor(_)) => selected.set(SelectedPanel.Summary)
          case _                               => Callback.empty
        }
      }
      .useStateViewWithReuse(TargetVisualOptions.Default)
      .useStateViewWithReuse(defaultLayout)
      .useEffectWithDepsBy((p, _, _, _) => p.focusedObs) { (props, panels, _, layout) =>
        implicit val ctx = props.ctx
        _ =>
          TabGridPreferencesQuery
            .queryWithDefault[IO](props.userId.get,
                                  GridLayoutSection.ObservationsLayout,
                                  ResizableSection.ObservationsTree,
                                  (Constants.InitialTreeWidth.toInt, defaultLayout)
            )
            .attempt
            .flatMap {
              case Right((w, l)) =>
                (panels
                  .mod(
                    TwoPanelState.treeWidth[Observation.Id].replace(w.toDouble)
                  ) *> layout.mod(o => mergeMap(o, l)))
                  .to[IO]
              case Left(_)       => IO.unit
            }
            .runAsync
      }
      .useResizeDetector()
      .useSingleEffect(debounce = 1.second)
      .renderWithReuse { (props, panels, options, layouts, resize, debouncer) =>
        implicit val ctx = props.ctx
        ObsLiveQuery(
          Reuse(renderFn _)(props, panels, options, layouts, resize, debouncer)
        )
      }

}
