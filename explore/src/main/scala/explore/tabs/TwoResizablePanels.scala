// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.AppTab
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.scalajs.dom.window
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import react.draggable.Axis
import react.resizable.*
import react.resizeDetector.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes.*

enum RightSideCardinality:
  case Single, Multi

trait TwoResizablePanels {
  def coreDimensions(resize: UseResizeDetectorReturn, treeWidth: Int): (Int, Int) =
    val coreWidth =
      if (window.canFitTwoPanels) {
        resize.width.getOrElse(0)
      } else {
        resize.width.getOrElse(0) - treeWidth
      }

    val coreHeight = resize.height.getOrElse(0)
    (coreWidth, coreHeight)

  def treeResize(
    userId:    Option[User.Id],
    widthView: View[Double],
    section:   ResizableSection,
    debouncer: Reusable[UseSingleEffect[IO]]
  )(using TransactionalClient[IO, UserPreferencesDB], Logger[IO]) =
    (_: ReactEvent, d: ResizeCallbackData) =>
      widthView.set(d.size.width.toDouble) *>
        debouncer
          .submit(
            UserWidthsCreation
              .storeWidthPreference[IO](userId, section, d.size.width)
          )
          .runAsync

  def makeBackButton(
    programId: Program.Id,
    appTab:    AppTab,
    pv:        View[SelectedPanel],
    ctx:       AppContext[IO]
  ): VdomNode =
    Button(
      as = <.a,
      size = Mini,
      compact = true,
      basic = true,
      clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
      onClickE = linkOverride[ButtonProps](
        ctx.pushPage(appTab, programId, Focused.None) >>
          pv.set(SelectedPanel.tree)
      )
    )(^.href := ctx.pageUrl(appTab, programId, Focused.None), Icons.ChevronLeft)

  def tree(panel: VdomNode, treeWidth: Int, cardinality: RightSideCardinality) =
    <.div(
      ^.width := treeWidth.px,
      ExploreStyles.Tree,
      ExploreStyles.ResizableSinglePanel.when(cardinality == RightSideCardinality.Single),
      ExploreStyles.ResizableMultiPanel.when(cardinality == RightSideCardinality.Multi),
      treeInner(panel)
    )

  def treeInner(panel: VdomNode): VdomNode =
    <.div(ExploreStyles.TreeBody)(panel)

  def makeOneOrTwoPanels(
    treeWidth:   Int,
    coreHeight:  Int,
    coreWidth:   Int,
    pv:          View[TwoPanelState],
    leftPanel:   VdomNode,
    rightSide:   VdomNode,
    cardinality: RightSideCardinality,
    treeResize:  (ReactEvent, ResizeCallbackData) => Callback
  ): VdomNode =
    if (window.canFitTwoPanels) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(leftPanel))
          .when(pv.get.selected.leftPanelVisible),
        <.div(ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(pv.get.selected.rightPanelVisible)
      )
    } else {
      <.div(
        ExploreStyles.TreeRGL,
        Resizable(
          axis = Axis.X,
          width = treeWidth.toDouble,
          height = coreHeight.toDouble,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (coreWidth / 2, 0),
          onResize = treeResize,
          resizeHandles = List(ResizeHandleAxis.East),
          content = tree(leftPanel, treeWidth, cardinality),
          clazz = ExploreStyles.ResizableSeparator
        ),
        <.div(
          ExploreStyles.SinglePanelTile,
          ^.width := coreWidth.px,
          ^.left  := treeWidth.px
        )(
          rightSide
        )
      )
    }
}
