// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.data.NonEmptyList
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegInt
import explore.GraphQLSchemas.ObservationDB
import explore.common.UserPreferencesQueries._
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.components.{ Tile, TileButton }
import explore.implicits._
import explore.model.Focused.FocusedObs
import explore.model._
import explore.model.enum.{ AppTab, TileSizeState }
import explore.model.layout._
import explore.model.layout.unsafe._
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.observationtree.ObsQueries._
import explore.target.TargetQueries._
import explore.targeteditor.TargetBody
import explore.{ AppCtx, Icons }
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.User
import lucuma.core.model.Target
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.macros.Lenses
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._
import react.sizeme._

import scala.concurrent.duration._

final case class ObsTabContents(
  userId:  ViewOpt[User.Id],
  focused: View[Option[Focused]]
) extends ReactProps[ObsTabContents](ObsTabContents.component) {
  def isObsSelected: Boolean = focused.get.isDefined
}

object ObsTabContents {
  val NotesIndex: NonNegInt     = 0
  val NotesMaxHeight: NonNegInt = 3
  val NotesMinHeight: NonNegInt = 1

  private val layoutLarge: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = 12,
                 h = NotesMaxHeight.value,
                 i = "notes",
                 isResizable = false,
                 resizeHandles = List("")
      ),
      LayoutItem(x = 0, y = NotesMaxHeight.value, w = 12, h = 8, i = "target")
    )
  )

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = 12,
                 h = NotesMaxHeight.value,
                 i = "notes",
                 isResizable = false,
                 resizeHandles = List("")
      ),
      LayoutItem(x = 0, y = NotesMaxHeight.value, w = 12, h = 8, i = "target")
    )
  )

  private val layoutSmall: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = 12,
                 h = NotesMaxHeight.value,
                 i = "notes",
                 isResizable = false,
                 resizeHandles = List("")
      ),
      LayoutItem(x = 0, y = NotesMaxHeight.value, w = 12, h = 8, i = "target")
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

  @Lenses
  final case class State(
    panels:  TwoPanelState,
    layouts: LayoutsMap,
    options: TargetVisualOptions
  ) {
    def updateLayouts(newLayouts: LayoutsMap): State =
      copy(layouts = mergeMap(layouts, newLayouts))
  }

  object State {
    val panelsWidth   = State.panels.composeLens(TwoPanelState.treeWidth)
    val panelSelected = State.panels.composeLens(TwoPanelState.elementSelected)

    def breakPointNote(n: BreakpointName) =
      layouts.breakPointLayout(n, NotesIndex)
    def notesHeight =
      breakPointNote(BreakpointName.sm).composeLens(layoutItemHeight)
    def notesHeightState(s: State): TileSizeState = notesHeight.getOption(s) match {
      case Some(x) if x === NotesMinHeight.value => TileSizeState.Minimized
      case _                                     => TileSizeState.Normal
    }
  }

  type Props = ObsTabContents
  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def readLayoutPreference(userId: Option[User.Id]): Callback =
      AppCtx
        .flatMap { implicit ctx =>
          UserGridLayoutQuery
            .queryWithDefault[IO](userId, GridLayoutSection.ObservationsLayout, defaultLayout)
        }
        .runAsyncAndThenCB(l => $.setStateL(State.layouts)(l))

    def readWidthPreference(userId: Option[User.Id]): Callback =
      AppCtx
        .flatMap { implicit ctx =>
          UserAreaWidths
            .queryWithDefault[IO](userId,
                                  ResizableSection.ObservationsTree,
                                  Constants.InitialTreeWidth.toInt
            )
        }
        .runAsyncAndThenCB(w => $.setStateL(State.panelsWidth)(w))

    def render(props: Props, state: State) = {
      AppCtx.withCtx { implicit ctx =>
        val treeResize =
          (_: ReactEvent, d: ResizeCallbackData) =>
            $.setStateL(State.panelsWidth)(d.size.width) *>
              UserWidthsCreation
                .storeWidthPreference[IO](props.userId.get,
                                          ResizableSection.ObservationsTree,
                                          d.size.width
                )
                .runAsyncAndForgetCB
                .debounce(1.second)

        val treeWidth = state.panels.treeWidth.toDouble

        // Tree area
        def tree(observations: View[List[ObsSummary]]) =
          <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableMultiPanel)(
            treeInner(observations)
          )

        def treeInner(observations: View[List[ObsSummary]]) =
          <.div(ExploreStyles.TreeBody)(
            ObsList(
              observations,
              props.focused
            )
          )

        def storeLayouts(layouts: Layouts): Callback =
          UserGridLayoutUpsert
            .storeLayoutsPreference[IO](props.userId.get,
                                        GridLayoutSection.ObservationsLayout,
                                        layouts
            )
            .runAsyncAndForgetCB
            .debounce(1.second)

        SizeMe() { s =>
          ObsLiveQuery { observations =>
            val obsSummaryOpt = props.focused.get.collect { case FocusedObs(obsId) =>
              observations.get.find(_.id === obsId)
            }.flatten

            val backButton = TileButton(
              Button(
                as = <.a,
                basic = true,
                size = Mini,
                compact = true,
                clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
                onClickE = linkOverride[IO, ButtonProps](props.focused.set(none))
              )(^.href := ctx.pageUrl(AppTab.Observations, none), Icons.ChevronLeft.fitted(true))
            )

            // Use a fixed target id until observations have a real target
            val targetId = Target.Id(2L)

            val coreWidth =
              if (window.innerWidth <= Constants.TwoPanelCutoff) {
                s.width.toDouble
              } else {
                s.width.toDouble - treeWidth
              }
            val rightSide = ResponsiveReactGridLayout(
              width = coreWidth.toInt,
              margin = (5, 5),
              containerPadding = (5, 0),
              rowHeight = Constants.GridRowHeight,
              draggableHandle = s".${ExploreStyles.TileTitleMenu.htmlClass}",
              onLayoutChange = (_: Layout, b: Layouts) => storeLayouts(b),
              layouts = state.layouts
            )(
              <.div(
                ^.key := "notes",
                Tile(
                  s"Note for Observer ${obsSummaryOpt}",
                  backButton.some,
                  canMinimize = true,
                  canMaximize = true,
                  state = State.notesHeightState(state),
                  sizeStateCallback = (s: TileSizeState) =>
                    $.setStateL(State.notesHeight)(s match {
                      case TileSizeState.Minimized => 1
                      case _                       => 3
                    })
                )(
                  <.div(
                    ExploreStyles.NotesWrapper,
                    <.div(
                      ExploreStyles.ObserverNotes,
                      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
                    )
                  )
                )
              ),
              <.div(
                ^.key := "target",
                Tile("Target")(
                  LiveQueryRenderMod[ObservationDB,
                                     TargetEditQuery.Data,
                                     Option[TargetEditQuery.Data.Target]
                  ](
                    TargetEditQuery.query(targetId),
                    _.target,
                    NonEmptyList.of(TargetEditSubscription.subscribe[IO](targetId))
                  ) { targetOpt =>
                    <.div(
                      <.div(
                        (props.userId.get, targetOpt.get).mapN { case (uid, _) =>
                          val stateView = ViewF.fromState[IO]($).zoom(State.options)
                          TargetBody(uid, targetId, targetOpt.zoom(_.get)(f => _.map(f)), stateView)
                        }
                      ).when(false)
                    )
                  }
                )
              )
            )

            if (window.innerWidth <= Constants.TwoPanelCutoff) {
              <.div(
                ExploreStyles.TreeRGL,
                <.div(ExploreStyles.Tree, treeInner(observations))
                  .when(state.panels.leftPanelVisible),
                <.div(ExploreStyles.SinglePanelTile, rightSide)
                  .when(state.panels.rightPanelVisible)
              )
            } else {
              <.div(
                ExploreStyles.TreeRGL,
                Resizable(
                  axis = Axis.X,
                  width = treeWidth,
                  height = Option(s.height).getOrElse(0),
                  minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
                  maxConstraints = (s.width.toInt / 2, 0),
                  onResize = treeResize,
                  resizeHandles = List(ResizeHandleAxis.East),
                  content = tree(observations)
                ),
                <.div(^.width := coreWidth.px,
                      ^.left := treeWidth.px,
                      ExploreStyles.SinglePanelTile
                )(
                  <.div(ExploreStyles.TreeRGLWrapper, rightSide)
                )
              )
            }
          }
        }
      }
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState((p, s: Option[State]) =>
        s match {
          case None    =>
            State(TwoPanelState.initial(p.isObsSelected),
                  defaultLayout,
                  TargetVisualOptions.Default
            )
          case Some(s) =>
            if (s.panels.elementSelected =!= p.isObsSelected)
              State.panelSelected.set(p.isObsSelected)(s)
            else s
        }
      )
      .renderBackend[Backend]
      .componentDidMount($ =>
        $.backend.readWidthPreference($.props.userId.get) *>
          $.backend.readLayoutPreference($.props.userId.get)
      )
      .configure(Reusability.shouldComponentUpdate)
      .build

}
