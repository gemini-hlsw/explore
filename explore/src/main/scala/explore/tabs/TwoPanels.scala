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
import react.resizeDetector.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes.*

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

  private def tree(panel: VdomNode, cardinality: RightSideCardinality) =
    <.div(
      ExploreStyles.Tree,
      treeInner(panel)
    )

  private def treeInner(panel: VdomNode): VdomNode =
    <.div(ExploreStyles.TreeBody)(panel)

  def makeOneOrTwoPanels(
    pv:          View[TwoPanelState],
    leftPanel:   VdomNode,
    rightSide:   UseResizeDetectorReturn => VdomNode,
    cardinality: RightSideCardinality,
    resize:      UseResizeDetectorReturn
  ): VdomNode =
    if (window.canFitTwoPanels) {
      <.div(
        ExploreStyles.TreeRGL,
        tree(leftPanel, cardinality),
        <.div(
          ExploreStyles.SinglePanelTile.when(cardinality == RightSideCardinality.Single),
          ExploreStyles.MultiPanelTile.when(cardinality == RightSideCardinality.Multi)
        )(
          rightSide(resize)
        ).withRef(resize.ref) // we want to measure the grid layout
      )
    } else {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(leftPanel))
          .when(pv.get.selected.leftPanelVisible),
        <.div(
          ExploreStyles.SinglePanelTile.when(cardinality == RightSideCardinality.Single),
          ExploreStyles.MultiPanelTile.when(cardinality == RightSideCardinality.Multi)
        )(
          rightSide(resize)
        ).when(pv.get.selected.rightPanelVisible)
      ).withRef(resize.ref)
    }
}
