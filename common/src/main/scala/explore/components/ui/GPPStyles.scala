// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.ui

import cats.syntax.all._
import react.common.implicits._
import react.common.style._

object GPPStyles {
  val GPPForm: Css      = Css("gpp-form")
  val GPPTile: Css      = Css("gpp-tile")
  val GPPTileTitle: Css = Css("gpp-tile-title")
  val GPPTileBody: Css  = Css("gpp-tile-body")

  // re-evaluate the need/use of this as part of overall
  // styling and layout
  val ProposalTile: Css = Css("proposal-tile")

  val MainGrid: Css   = Css("main-grid")
  val MainHeader: Css = Css("main-header")
  val MainBody: Css   = Css("main-body")
  val MainTitle: Css  = Css("main-title")
  val SideTabs: Css   = Css("sidetabs")

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

  val AladinColumn: Css          = Css("aladin-column")
  val AladinContainerColumn: Css = Css("aladin-container-column")
  val AladinContainerBody: Css   = Css("aladin-container-body")
  val AladinContainerStatus: Css = Css("aladin-container-status")
  val AladinFOV: Css             = Css("aladin-status-fov")
  val AladinDetailText: Css      = Css("aladin-detail-text")
  val AladinCurrentCoords: Css   = Css("aladin-status-current-coordinates")
  val AladinCenterButton: Css    = Css("aladin-status-center-button")

  val MoonPhase: Css = Css("moon-phase")

  // Semantic UI form overrides and extensions
  val Grid: Css            = Css("gpp-grid")
  val TwoColumnGrid: Css   = Grid |+| Css("gpp-two-columns")
  val ThreeColumnGrid: Css = Grid |+| Css("gpp-three-columns")
  val CellWrapper: Css     = Css("gpp-cell-wrapper")

  val GrowOne: Css   = Css("gpp-grow-one")
  val GrowTwo: Css   = Css("gpp-grow-two")
  val GrowThree: Css = Css("gpp-grow-three")
  val GrowFour: Css  = Css("gpp-grow-four")

  // Move an element to the end of the flex container
  val ToEnd: Css = Css("gpp-to-end")

  // Hide a label while keeping it accessible to screen readers
  val HideLabel: Css = Css("hide-label")

  // Change styles for a readonly input
  val StaticData: Css = Css("static-data")
}
