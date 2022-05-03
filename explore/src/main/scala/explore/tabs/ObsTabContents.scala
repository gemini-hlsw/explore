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
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
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

import java.time.LocalDateTime
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

final case class ObsTabContents(
  userId:           ReuseViewOpt[User.Id],
  programId:        Program.Id,
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
  private val TargetHeight: NonNegInt           = 18
  private val TargetMinHeight: NonNegInt        = 15
  private val ElevationPlotMinHeight: NonNegInt = 8
  private val ConstraintsMaxHeight: NonNegInt   = 6
  private val ConfigurationMaxHeight: NonNegInt = 10
  private val DefaultWidth: NonNegInt           = 10
  private val DefaultLargeWidth: NonNegInt      = 12

  private val layoutLarge: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultLargeWidth.value,
                 h = NotesMaxHeight.value,
                 i = ObsTabTiles.NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultLargeWidth.value,
                 h = TargetHeight.value,
                 minH = TargetMinHeight.value,
                 i = ObsTabTiles.TargetId.value
      ),
      LayoutItem(x = 0,
                 y = (NotesMaxHeight |+| TargetHeight).value,
                 w = DefaultLargeWidth.value,
                 h = ElevationPlotMinHeight.value,
                 i = ObsTabTiles.PlotId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| ElevationPlotMinHeight).value,
        w = DefaultLargeWidth.value,
        h = ConstraintsMaxHeight.value,
        i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetHeight |+| ElevationPlotMinHeight |+| ConstraintsMaxHeight).value,
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
                 h = TargetHeight.value,
                 minH = TargetMinHeight.value,
                 i = ObsTabTiles.TargetId.value
      ),
      LayoutItem(x = 0,
                 y = (NotesMaxHeight |+| TargetHeight).value,
                 w = DefaultWidth.value,
                 h = ElevationPlotMinHeight.value,
                 i = ObsTabTiles.PlotId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetHeight |+| ElevationPlotMinHeight).value,
        w = DefaultWidth.value,
        h = ConstraintsMaxHeight.value,
        i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y =
          (NotesMaxHeight |+| TargetHeight |+| ElevationPlotMinHeight |+| ConstraintsMaxHeight).value,
        w = DefaultWidth.value,
        h = ConfigurationMaxHeight.value,
        i = ObsTabTiles.ConfigurationId.value
      )
    )
  )

  private val defaultObsLayouts: LayoutsMap =
    defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutLarge)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  type Props = ObsTabContents
  implicit val propsReuse: Reusability[Props] = Reusability.derive

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
    targetObsMap: SortedMap[Target.Id, TargetSummary],
    obsId:        Observation.Id,
    targetId:     Target.Id
  ): Int =
    targetObsMap.get(targetId).fold(0)(summary => (summary.obsIds - obsId).size)

  protected def renderFn(
    props:              Props,
    panels:             ReuseView[TwoPanelState],
    defaultLayouts:     LayoutsMap,
    layouts:            ReuseView[LayoutsMap],
    resize:             UseResizeDetectorReturn,
    obsConf:            ReuseView[ObsConfiguration],
    debouncer:          Reusable[UseSingleEffect[IO]],
    obsWithConstraints: ReuseView[ObsSummariesWithConstraints]
  )(implicit ctx:       AppContextIO): VdomNode = {
    val observations     = obsWithConstraints.zoom(ObsSummariesWithConstraints.observations)
    val constraintGroups = obsWithConstraints.zoom(ObsSummariesWithConstraints.constraintGroups)

    val panelsResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        panels.zoom(TwoPanelState.treeWidth).set(d.size.width.toDouble) *>
          debouncer
            .submit(
              UserWidthsCreation
                .storeWidthPreference[IO](props.userId.get,
                                          ResizableSection.ObservationsTree,
                                          d.size.width
                )
            )
            .runAsync

    val treeWidth    = panels.get.treeWidth.toInt
    val selectedView = panels.zoom(TwoPanelState.selected)

    // Tree area
    def tree(observations: ReuseView[ObservationList]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableMultiPanel)(
        treeInner(observations)
      )

    def treeInner(observations: ReuseView[ObservationList]) =
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

    val targetCoords: Option[Coordinates] =
      props.focusedTarget.flatMap(obsWithConstraints.get.targetMap.get).flatMap(_.coords)

    val localConf = obsConf.get.configuration

    // if no config defined use the one from the query, else prefer the local
    val conf: Option[ObsConfiguration] = observations.get.collect {
      case (i, (ObsSummaryWithTitleConstraintsAndConf(_, _, _, _, _, _, _, Some(c)), _))
          if props.focusedObs.exists(_ === i) && localConf.isEmpty =>
        obsConf.get.copy(configuration = c.some)
      case _ => obsConf.get
    }.headOption

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        basic = true,
        size = Mini,
        compact = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](
          ctx.pushPage(AppTab.Observations, props.programId, none, none) >>
            selectedView.set(SelectedPanel.tree)
        )
      )(^.href := ctx.pageUrl(AppTab.Observations, props.programId, none, none), Icons.ChevronLeft)
    )

    val coreWidth =
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
           resize,
           coreWidth,
           conf,
           defaultObsLayouts,
           layouts,
           notesTile,
           targetCoords,
           props.undoStacks,
           props.searching,
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
            programId: Program.Id,
            oid:       Option[Observation.Id],
            tid:       Option[Target.Id],
            via:       SetRouteVia
          ): Callback =
            ctx.setPageVia(AppTab.Observations, programId, oid.map(ObsIdSet.one(_)), tid, via)

          val targetTile = AsterismEditorTile.asterismEditorTile(
            props.userId.get,
            props.programId,
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
            conf,
            props.focusedTarget,
            Reuse(setCurrentTarget _)(props.programId, props.focusedObs),
            Reuse.currying(obsWithConstraints.get.targetMap, obsId).in(otherObsCount _),
            props.undoStacks.zoom(ModelUndoStacks.forSiderealTarget),
            props.searching,
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
            obsConf,
            obsView.map(_.zoom(scienceDataForObs)),
            props.undoStacks
              .zoom(ModelUndoStacks.forScienceData[IO])
              .zoom(atMapWithDefault(obsId, UndoStacks.empty))
          )

          TileController(
            props.userId.get,
            coreWidth,
            defaultLayouts,
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
          )
        }
      )

    val rightSide: VdomNode =
      props.focusedObs.fold[VdomNode](
        Tile("observations", "Observations Summary", backButton.some, key = "observationsSummary")(
          Reuse.by(obsWithConstraints)((_: Tile.RenderInTitle) =>
            <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                  <.div("Select or add an observation")
            )
          )
        )
      )(rightSideRGL)

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
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse(TwoPanelState.initial(SelectedPanel.Uninitialized))
      .useEffectWithDepsBy((props, panels) =>
        (props.focusedObs, panels.zoom(TwoPanelState.selected).value.reuseByValue)
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
      .useStateViewWithReuse(defaultObsLayouts)
      // Keep a record of the initial target layouut
      .useMemo(())(_ => defaultObsLayouts)
      // Restore positions from the db
      .useEffectWithDepsBy((p, _, _, _, _) => p.focusedObs) {
        (props, panels, _, layout, defaultLayout) =>
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
                      TwoPanelState.treeWidth.replace(w.toDouble)
                    ) *> layout.mod(o => mergeMap(o, l)))
                    .to[IO]
                case Left(_)       => IO.unit
              }
              .runAsync
      }
      // Shared obs conf (posAngle/obsTime)
      .useStateViewWithReuse(ObsConfiguration(PosAngle.Default, LocalDateTime.now(), none))
      .useSingleEffect(debounce = 1.second)
      .renderWithReuse {
        (props, twoPanelState, resize, layouts, defaultLayout, obsConf, debouncer) =>
          implicit val ctx = props.ctx
          <.div(
            ObsLiveQuery(
              props.programId,
              Reuse(renderFn _)(props,
                                twoPanelState,
                                defaultLayout,
                                layouts,
                                resize,
                                obsConf,
                                debouncer
              )
            )
          ).withRef(resize.ref)
      }

}
