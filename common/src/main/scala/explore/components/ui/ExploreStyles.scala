// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.ui

import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import react.common.implicits._
import react.common.style._

object ExploreStyles {
  val Tile: Css      = Css("explore-tile")
  val TileTitle: Css = Css("explore-tile-title")
  val TileBody: Css  = Css("explore-tile-body")

  // re-evaluate the need/use of this as part of overall
  // styling and layout
  val ProposalTile: Css = Css("proposal-tile")

  val MainGrid: Css   = Css("main-grid")
  val MainHeader: Css = Css("main-header")
  val MainBody: Css   = Css("main-body")
  val MainTitle: Css  = Css("main-title")
  val SideTabs: Css   = Css("sidetabs")
  val SideButton: Css = Css("side-button")

  val ResizeHandle: Css    = Css("resize-handle")
  val Tree: Css            = Css("tree")
  val TreeBodyOuter: Css   = Css("tree-body-outer")
  val TreeBodyInner: Css   = Css("tree-body-inner")
  val TreeRGL: Css         = Css("tree-rgl")
  val RGLBody: Css         = Css("rgl-body")
  val RGLArea: Css         = Css("rgl-area")
  val SinglePanelArea: Css = Css("single-panel-area")
  val SinglePanelTile: Css = Css("single-panel-tile")

  val SideTabsBody: Css         = Css("sidetabs-body")
  val VerticalButton: Css       = Css("vertical-button")
  val RotationWrapperOuter: Css = Css("rotation-wrapper-outer")
  val RotationWrapperInner: Css = Css("rotation-wrapper-inner")

  val HVCenter: Css        = Css("horizontal-vertical-center")
  val FullHeightWidth: Css = Css("full-height-width")

  val ObsTree: Css            = Css("obs-tree")
  val ObsTreeGroup: Css       = Css("obs-tree-group")
  val ObsTreeGroupHeader: Css = Css("obs-tree-group-header")
  val ObsTreeItem: Css        = Css("obs-tree-item")
  val ObsTreeButtons: Css     = Css("obs-tree-buttons")
  val ObsBadge: Css           = Css("obs-badge")
  val ObsItem: Css            = Css("obs-item")

  val DraggingOver: Css = Css("dragging-over")

  val SkyPlot: Css         = Css("sky-plot")
  val SkyPlotSection: Css  = Css("sky-plot-section")
  val SkyPlotControls: Css = Css("sky-plot-controls")
  val PlotToggle: Css      = Css("plot-toggle")

  val AladinContainerColumn: Css = Css("aladin-container-column")
  val AladinContainerBody: Css   = Css("aladin-container-body")
  val AladinFOV: Css             = Css("aladin-status-fov")
  val AladinDetailText: Css      = Css("aladin-detail-text")
  val AladinCurrentCoords: Css   = Css("aladin-status-current-coordinates")
  val AladinCenterButton: Css    = Css("aladin-status-center-button")

  val MoonPhase: Css = Css("moon-phase")

  val PartnerSplitTotal: Css = Css("partner-split-total")
  val PartnerSplitData: Css  = Css("partner-split-data")
  val PartnerSplitFlag: Css  = Css("partner-split-flag")

  val PartnerSplitsEditorError: Css = Css("partner-splits-editor-error")
  val PartnerSplitsEditorTable: Css = Css("partner-splits-editor-table")

  // Semantic UI form overrides and extensions
  val Grid: Css            = Css("explore-grid")
  val TwoColumnGrid: Css   = Grid |+| Css("explore-two-columns")
  val ThreeColumnGrid: Css = Grid |+| Css("explore-three-columns")
  val Compact: Css         = Css("explore-compact")
  val ErrorLabel: Css      = Css("explore-error-label")

  val TextInForm: Css = Css("explore-text-in-form")

  val FlexContainer: Css = Css("explore-flex-container")
  val FlexWrap: Css      = Css("explore-flex-wrap")

  def FlexGrow(i:   Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-grow-$i")
  def FlexShrink(i: Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-shrink-$i")

  // Move an element to the end of the flex container
  val FlexEnd: Css = Css("explore-flex-end")

  // Hide a label while keeping it accessible to screen readers
  val HideLabel: Css = Css("hide-label")

  // Change styles for a readonly input
  val StaticData: Css = Css("static-data")

  // Clue websocket connection status
  val ConnectionOK: Css      = Css("connection-ok")
  val ConnectionWarning: Css = Css("connection-warning")
  val ConnectionError: Css   = Css("connection-error")

  // WIP
  val WIP: Css        = Css("wip")
  val WIPWarning: Css = Css("wip-warning")

  // The Target tab contents
  val TargetGrid: Css          = Css("target-grid")
  val TargetAladinCell: Css    = Css("target-aladin-cell")
  val TargetSkyplotCell: Css   = Css("target-skyplot-cell")
  val TargetRaDecMinWidth: Css = Css("target-ra-dec-min-width")
  val CoordinatesForm: Css     = Css("coordinates-form")
  val CatalogueForm: Css       = Css("catalogue-form")

  // styles for icons that acts as buttons
  val ButtonIcon: Css = Css("explore-button-icon")
}
