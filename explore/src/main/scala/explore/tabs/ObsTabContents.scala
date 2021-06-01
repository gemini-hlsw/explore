// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.ViewF
import crystal.implicits._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ObsQueries._
import explore.common.ObsQueriesGQL._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.TileController
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Focused.FocusedObs
import explore.model._
import explore.model.enum.AppTab
import explore.model.layout._
import explore.model.layout.unsafe._
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.schemas.ObservationDB
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.macros.Lenses
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.resizeDetector.ResizeDetector
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.concurrent.duration._

final case class ObsTabContents(
  userId:           ViewOpt[User.Id],
  focused:          View[Option[Focused]],
  searching:        View[Set[Target.Id]],
  size:             ResizeDetector.Dimensions
)(implicit val ctx: AppContextIO)
    extends ReactProps[ObsTabContents](ObsTabContents.component) {
  def isObsSelected: Boolean = focused.get.collect { case Focused.FocusedObs(_) => () }.isDefined
}

object ObsTabTiles {
  val NotesId: NonEmptyString         = "notes"
  val TargetId: NonEmptyString        = "target"
  val ConstraintsId: NonEmptyString   = "constraints"
  val ConfigurationId: NonEmptyString = "configuration"
}

object ObsTabContents {
  val NotesMaxHeight: NonNegInt         = 3
  val NotesMinHeight: NonNegInt         = 1
  val TargetMinHeight: NonNegInt        = 12
  val ConstraintsMaxHeight: NonNegInt   = 6
  val ConstraintsMinHeight: NonNegInt   = 1
  val ConfigurationMaxHeight: NonNegInt = 8
  val ConfigurationMinHeight: NonNegInt = 1
  val DefaultWidth: NonNegInt           = 12

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
                 h = ConstraintsMaxHeight.value,
                 i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetMinHeight |+| ConstraintsMaxHeight).value,
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
                 h = ConstraintsMaxHeight.value,
                 i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetMinHeight |+| ConstraintsMaxHeight).value,
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
                 h = ConstraintsMaxHeight.value,
                 i = ObsTabTiles.ConstraintsId.value
      ),
      LayoutItem(
        x = 0,
        y = (NotesMaxHeight |+| TargetMinHeight |+| ConstraintsMaxHeight).value,
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
    val fovAngle      = State.options.composeLens(TargetVisualOptions.fovAngle)
  }

  type Props = ObsTabContents
  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  protected class Backend($ : BackendScope[Props, State]) {
    def readTabPreference(userId: Option[User.Id])(implicit ctx: AppContextIO): Callback =
      ObsTabPreferencesQuery
        .queryWithDefault[IO](userId,
                              ResizableSection.ObservationsTree,
                              (Constants.InitialTreeWidth.toInt, defaultLayout)
        )
        .runAsyncAndThenCB {
          case Right((w, l)) =>
            $.modState((s: State) => State.panelsWidth.set(w)(s.updateLayouts(l)))
          case Left(_)       => Callback.empty
        }

    // TODO Use this method
    def readTargetPreferences(targetId: Target.Id)(implicit ctx: AppContextIO): Callback =
      $.props.flatMap { p =>
        p.userId.get.map { uid =>
          UserTargetPreferencesQuery
            .queryWithDefault[IO](uid, targetId, Constants.InitialFov)
            .flatMap(v => $.modStateIn[IO](State.fovAngle.set(v)))
            .runAsyncAndForgetCB
        }.getOrEmpty
      }

    protected def renderFn(
      props:        Props,
      state:        View[State],
      observations: View[ObservationList]
    )(implicit ctx: AppContextIO): VdomNode = {

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

      val treeWidth = state.get.panels.treeWidth.toInt

      // Tree area
      def tree(observations: View[ObservationList]) =
        <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableMultiPanel)(
          treeInner(observations)
        )

      def treeInner(observations: View[ObservationList]) =
        <.div(ExploreStyles.TreeBody)(
          ObsList(
            observations,
            props.focused
          )
        )

      val obsIdOpt: Option[Observation.Id] = props.focused.get.collect { case FocusedObs(obsId) =>
        obsId
      }

      val obsSummaryOpt: Option[ObsSummaryWithPointingAndConstraints] =
        obsIdOpt.flatMap(observations.get.getElement)

      val targetId = obsSummaryOpt.collect {
        case ObsSummaryWithPointingAndConstraints(_,
                                                  Some(Pointing.PointingTarget(tid, _)),
                                                  _,
                                                  _,
                                                  _
            ) =>
          tid
      }

      val backButton = Reuse.always[VdomNode](
        Button(
          as = <.a,
          basic = true,
          size = Mini,
          compact = true,
          clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
          onClickE = linkOverride[IO, ButtonProps](props.focused.set(none))
        )(^.href := ctx.pageUrl(AppTab.Observations, none), Icons.ChevronLeft.fitted(true))
      )

      val coreWidth =
        if (window.innerWidth <= Constants.TwoPanelCutoff) {
          props.size.width.getOrElse(0)
        } else {
          props.size.width.getOrElse(0) - treeWidth
        }

      val layouts = ViewF.fromState[IO]($).zoom(State.layouts)

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
            )
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
             targetId,
             props.searching,
             state.zoom(State.options)
            )
          ) { (obsViewPot: Pot[View[Pot[ObservationData]]]) =>
            val obsView: Pot[View[ObservationData]] =
              obsViewPot.flatMap(view =>
                view.get.map(obs => view.zoom(_ => obs)(mod => _.map(mod)))
              )

            TileController(
              props.userId.get,
              coreWidth,
              defaultLayout,
              layouts,
              List(
                notesTile,
                TargetTile.targetTile(props.userId.get,
                                      targetId,
                                      props.searching,
                                      state.zoom(State.options)
                ),
                ConstraintsTile
                  .constraintsTile(obsId, obsView.map(_.zoom(ObservationData.constraintSet))),
                ConfigurationTile.configurationTile(obsSummaryOpt.map(_.id))
              )
            ): VdomNode
          }
        ).withKey(obsId.toString)

      val rightSide =
        obsIdOpt.fold(
          <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                <.div("Select or add an observation")
          )
        )(obsId => <.div(ExploreStyles.TreeRGLWrapper, rightSideRGL(obsId)))

      if (window.innerWidth <= Constants.TwoPanelCutoff) {
        <.div(
          ExploreStyles.TreeRGL,
          <.div(ExploreStyles.Tree, treeInner(observations))
            .when(state.get.panels.leftPanelVisible),
          <.div(^.key := "obs-right-side", ExploreStyles.SinglePanelTile)(
            rightSide
          ).when(state.get.panels.rightPanelVisible)
        )
      } else {
        <.div(
          ExploreStyles.TreeRGL,
          Resizable(
            axis = Axis.X,
            width = treeWidth,
            height = props.size.height.getOrElse[Int](0),
            minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
            maxConstraints = (props.size.width.getOrElse(0) / 2, 0),
            onResize = treeResize,
            resizeHandles = List(ResizeHandleAxis.East),
            content = tree(observations)
          ),
          <.div(^.key := "obs-right-side",
                ^.width := coreWidth.px,
                ^.left := treeWidth.px,
                ExploreStyles.SinglePanelTile
          )(
            rightSide
          )
        )
      }
    }

    def render(props: Props) = {
      implicit val ctx = props.ctx
      ObsLiveQuery(Reuse(renderFn _)(props, ViewF.fromState[IO]($)))
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
      .componentDidMount($ => $.backend.readTabPreference($.props.userId.get)($.props.ctx))
      .configure(Reusability.shouldComponentUpdate)
      .build

}
