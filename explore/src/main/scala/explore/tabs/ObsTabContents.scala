// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ConstraintSetObsQueriesGQL.AssignConstraintSetToObs
import explore.common.ConstraintsQueriesGQL._
import explore.common.ObsQueries._
import explore.common.TargetQueriesGQL._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.TileController
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.implicits._
import explore.model.Focused.FocusedObs
import explore.model._
import explore.model.enum.AppTab
import explore.model.layout._
import explore.model.layout.unsafe._
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.schemas.ObservationDB
import explore.targeteditor.TargetBody
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.function.Field2.second
import monocle.macros.Lenses
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.resizeDetector.ResizeDetector
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.modules.dropdown.Dropdown
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

object ObsTabContents {
  val NotesId: NonEmptyString         = "notes"
  val TargetId: NonEmptyString        = "target"
  val ConstraintsId: NonEmptyString   = "constraints"
  val NotesMaxHeight: NonNegInt       = 3
  val NotesMinHeight: NonNegInt       = 1
  val TargetMinHeight: NonNegInt      = 12
  val ConstraintsMaxHeight: NonNegInt = 6
  val ConstraintsMinHeight: NonNegInt = 1
  val DefaultWidth: NonNegInt         = 12

  private val layoutLarge: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = NotesMaxHeight.value,
                 i = NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultWidth.value,
                 h = TargetMinHeight.value,
                 i = TargetId.value
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value + TargetMinHeight.value,
                 w = DefaultWidth.value,
                 h = ConstraintsMaxHeight.value,
                 i = ConstraintsId.value
      )
    )
  )

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = NotesMaxHeight.value,
                 i = NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultWidth.value,
                 h = TargetMinHeight.value,
                 i = TargetId.value
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value + TargetMinHeight.value,
                 w = DefaultWidth.value,
                 h = ConstraintsMaxHeight.value,
                 i = ConstraintsId.value
      )
    )
  )

  private val layoutSmall: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = NotesMaxHeight.value,
                 i = NotesId.value,
                 isResizable = false
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value,
                 w = DefaultWidth.value,
                 h = TargetMinHeight.value,
                 i = TargetId.value
      ),
      LayoutItem(x = 0,
                 y = NotesMaxHeight.value + TargetMinHeight.value,
                 w = DefaultWidth.value,
                 h = ConstraintsMaxHeight.value,
                 i = ConstraintsId.value
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

    private def constraintsSelectorFn(
      constraintsSetId: Option[ConstraintSet.Id],
      obsSummaryOpt:    Option[ObsSummary],
      observations:     ConstraintsInfo
    )(implicit ctx:     AppContextIO): VdomNode =
      Select(
        value = constraintsSetId.map(_.show).orEmpty, // Set to empty string to clear
        placeholder = "Select a constraint set",
        onChange = (a: Dropdown.DropdownProps) =>
          (obsSummaryOpt, ConstraintSet.Id.parse(a.value.toString)).mapN { (obsId, csId) =>
            AssignConstraintSetToObs
              .execute(csId, obsId.id)
              .runAsyncAndForgetCB *> Callback.log(s"Set to $csId")
          }.getOrEmpty,
        options = observations.map(s => new SelectItem(value = s.id.show, text = s.name.value))
      )

    protected def renderFn(
      props:        Props,
      state:        View[State],
      observations: View[(ConstraintsInfo, ObservationList)]
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

      val obsSummaryOpt: Option[ObsSummaryWithPointingAndConstraints] =
        props.focused.get.collect { case FocusedObs(obsId) =>
          observations.get._2.getElement(obsId)
        }.flatten

      val constraintsSetId = obsSummaryOpt.flatMap(_.constraints.map(_.id))

      val targetId = obsSummaryOpt.collect {
        case ObsSummaryWithPointingAndConstraints(_,
                                                  Some(Pointing.PointingTarget(tid, _)),
                                                  _,
                                                  _,
                                                  _
            ) =>
          tid
      }

      val backButton = Reusable.always[VdomNode](
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

      def targetRenderFn(
        targetId:      Target.Id,
        renderInTitle: Tile.RenderInTitle,
        targetOpt:     View[Option[TargetEditQuery.Data.Target]]
      ): VdomNode =
        (props.userId.get, targetOpt.get).mapN { case (uid, _) =>
          TargetBody(uid,
                     targetId,
                     targetOpt.zoom(_.get)(f => _.map(f)),
                     props.searching,
                     state.zoom(State.options),
                     renderInTitle
          )
        }

      def renderTarget(
        targetId:      Option[Target.Id],
        renderInTitle: Tile.RenderInTitle
      ): VdomNode =
        targetId
          .map[VdomNode] { targetId =>
            LiveQueryRenderMod[ObservationDB,
                               TargetEditQuery.Data,
                               Option[TargetEditQuery.Data.Target]
            ](
              TargetEditQuery.query(targetId),
              _.target,
              NonEmptyList.of(TargetEditSubscription.subscribe[IO](targetId))
            )((targetRenderFn _).reusable(targetId, renderInTitle))
              .withKey(s"target-$targetId")
          }
          .getOrElse(
            <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                  <.div("No target assigned")
            )
          )

      val targetTile =
        Tile(TargetId, "Target")((renderTarget _).reusable(targetId))

      def renderConstraintsFn(
        csId:          ConstraintSet.Id,
        renderInTitle: Tile.RenderInTitle,
        csOpt:         View[Option[ConstraintSetModel]]
      ): VdomNode =
        csOpt.get.map { _ =>
          <.div(
            ExploreStyles.ConstraintsObsTile,
            ConstraintsPanel(csId, csOpt.zoom(_.get)(f => _.map(f)), renderInTitle)
          )
        }

      def renderConstraints(
        constraintsSetId: Option[ConstraintSet.Id],
        renderInTitle:    Tile.RenderInTitle
      ): VdomNode =
        constraintsSetId
          .map[VdomNode] { csId =>
            LiveQueryRenderMod[ObservationDB, ConstraintSetQuery.Data, Option[ConstraintSetModel]](
              ConstraintSetQuery.query(csId),
              _.constraintSet,
              NonEmptyList.of(ConstraintSetEditSubscription.subscribe[IO](csId))
            )((renderConstraintsFn _).reusable(csId, renderInTitle))
              .withKey(s"constraint-$targetId")
          }
          .getOrElse(
            <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                  <.div("No constraints assigned")
            )
          )

      val constraintsTile =
        Tile(ConstraintsId,
             "Constraints",
             canMinimize = true,
             control = ((constraintsSelectorFn _)
               .reusable(constraintsSetId, obsSummaryOpt, observations.get._1))
               .some
        )(
          (renderConstraints _).reusable(constraintsSetId)
        )

      val notesTile =
        Tile(
          NotesId,
          s"Note for Observer",
          backButton.some,
          canMinimize = true
        )(
          Reusable.fn(_ =>
            <.div(
              ExploreStyles.NotesWrapper,
              <.div(
                ExploreStyles.ObserverNotes,
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
              )
            )
          )
        )

      val layouts = ViewF.fromState[IO]($).zoom(State.layouts)

      val rightSideRGL =
        TileController(
          props.userId.get,
          coreWidth,
          defaultLayout,
          layouts,
          List(notesTile, targetTile, constraintsTile)
        )

      val rightSide =
        if (props.focused.get.isDefined) {
          <.div(ExploreStyles.TreeRGLWrapper, rightSideRGL)
        } else {
          <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                <.div("Select or add an observation")
          )
        }

      if (window.innerWidth <= Constants.TwoPanelCutoff) {
        <.div(
          ExploreStyles.TreeRGL,
          <.div(ExploreStyles.Tree, treeInner(observations.zoom(second)))
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
            content = tree(observations.zoom(second))
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
      ObsLiveQuery((renderFn _).reusable(props, ViewF.fromState[IO]($)))
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
