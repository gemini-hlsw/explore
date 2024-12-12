// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.AppContext
import explore.model.enums.AppTab
import explore.model.enums.SelectedPanel
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.Css
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.window

enum RightSideCardinality:
  case Single, Multi

trait TwoPanels {

  def makeBackButton(
    programId: Program.Id,
    appTab:    AppTab,
    pv:        View[SelectedPanel],
    ctx:       AppContext[IO]
  ): VdomNode =
    Button(
      icon = Icons.ChevronLeft,
      severity = Button.Severity.Secondary,
      text = true,
      clazz = ExploreStyles.TileBackButton,
      onClick = ctx.pushPage((appTab, programId, Focused.None).some) >>
        pv.set(SelectedPanel.Tree)
    ).mini.compact

  private def tree(panel: VdomNode, extraCss: Css) =
    <.div(
      ExploreStyles.Tree,
      treeInner(panel, extraCss)
    )

  private def treeInner(panel: VdomNode, extraCss: Css): VdomNode =
    <.div(ExploreStyles.TreeBody |+| extraCss)(panel)

  def makeOneOrTwoPanels(
    pv:           View[SelectedPanel],
    leftPanel:    VdomNode,
    rightSide:    UseResizeDetectorReturn => VdomNode,
    cardinality:  RightSideCardinality,
    resize:       UseResizeDetectorReturn,
    bodyExtraCss: Css = Css.Empty
  ): VdomNode =
    if (window.canFitTwoPanels)
      <.div(
        ExploreStyles.TreeRGL,
        tree(leftPanel, bodyExtraCss),
        <.div(
          ExploreStyles.SinglePanelTile.when(cardinality == RightSideCardinality.Single),
          ExploreStyles.MultiPanelTile.when(cardinality == RightSideCardinality.Multi)
        )(
          rightSide(resize)
        ).withRef(resize.ref) // we want to measure the grid layout
      )
    else
      <.div(ExploreStyles.TreeRGL)(
        <.div(ExploreStyles.Tree, treeInner(leftPanel, bodyExtraCss))
          .when(pv.get.leftPanelVisible),
        <.div(
          ExploreStyles.SinglePanelTile.when(cardinality == RightSideCardinality.Single),
          ExploreStyles.MultiPanelTile.when(cardinality == RightSideCardinality.Multi)
        )(
          rightSide(resize)
        ).when(pv.get.rightPanelVisible)
      ).withRef(resize.ref)
}
