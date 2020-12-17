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

  val TextInForm: Css = Css("explore-text-in-form")

  val FlexContainer: Css = Css("explore-flex-container")
  val FlexWrap: Css      = Css("explore-flex-wrap")

  def FlexGrow(i:   Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-grow-$i")
  def FlexShrink(i: Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-shrink-$i")

  // Move an element to the end of the flex container
  val FlexEnd: Css = Css("explore-flex-end")

  def Grow(i:       Int Refined Interval.Closed[1, 4]): Css  = Css(s"explore-grow-$i")
  def ColumnSpan(i: Int Refined Interval.Closed[1, 16]): Css = Css(s"explore-column-span-$i")

  // Hide a label while keeping it accessible to screen readers
  val HideLabel: Css = Css("hide-label")

  // Change styles for a readonly input
  val StaticData: Css = Css("static-data")

  // re-evaluate the need/use of this as part of overall
  // styling and layout
  val ProposalTile: Css = Css("proposal-tile")

  val MainGrid: Css         = Css("main-grid")
  val MainHeader: Css       = Css("main-header")
  val MainBody: Css         = Css("main-body")
  val MainTitle: Css        = Css("main-title")
  val ConnectionIcon: Css   = Css("connection-icon")
  val MainUserName: Css     = Css("main-user-name")
  val MainMenu: Css         = Css("main-menu")
  val MainMenuDropdown: Css = Css("main-menu-dropdown")
  val SideTabs: Css         = Css("sidetabs")
  val SideButton: Css       = Css("side-button")
  val BlendedButton: Css    = Css("blended-button")
  val JustifyRight: Css     = Css("justify-right")
  val TargetLabelTitle: Css = Css("target-label-title")
  val DeleteButton: Css     = Css("delete-button")

  val ResizeHandle: Css       = Css("resize-handle")
  val ResizableSeparator: Css = Css("resize-separator")
  val Tree: Css               = Css("tree")
  val TreeBodyOuter: Css      = Css("tree-body-outer")
  val TreeBodyInner: Css      = Css("tree-body-inner")
  val TreeRGL: Css            = Css("tree-rgl")
  val RGLBody: Css            = Css("rgl-body")
  val RGLArea: Css            = Css("rgl-area")
  val RGLPlaceholder: Css     = Css("rgl-placeholder")
  val SinglePanelArea: Css    = Css("single-panel-area")
  val SinglePanelTile: Css    = Css("single-panel-tile")

  val SideTabsBody: Css         = Css("sidetabs-body")
  val VerticalButton: Css       = Css("vertical-button")
  val RotationWrapperOuter: Css = Css("rotation-wrapper-outer")
  val RotationWrapperInner: Css = Css("rotation-wrapper-inner")

  val HVCenter: Css        = Css("horizontal-vertical-center")
  val FullHeightWidth: Css = Css("full-height-width")

  val ObsTree: Css                = Css("obs-tree")
  val ObsTreeWrapper: Css         = Css("obs-tree-wrapper")
  val ObsScrollTree: Css          = Css("obs-scroll-tree")
  val ObsTreeGroup: Css           = Css("obs-tree-group")
  val ObsCount: Css               = Css("obs-count")
  val SelectedObsTreeGroup: Css   = Css("selected-obs-tree-group")
  val UnselectedObsTreeGroup: Css = Css("unselected-obs-tree-group")
  val ObsTreeGroupHeader: Css     = Css("obs-tree-group-header")
  val ObsTreeItem: Css            = Css("obs-tree-item")
  val ObsTreeButtons: Css         = Css("obs-tree-buttons")
  val ObsBadge: Css               = Css("obs-badge")
  val SelectedObsItem: Css        = Css("selected-obs-item")
  val ObsItem: Css                = Css("obs-item")
  val TrashIcon: Css              = Css("trash-icon")

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

  val ProposalDetailsGrid: Css = Css("proposal-details-grid")

  val MinimumPercent: Css = Css("minimum-percent") |+| FlexEnd

  val PartnerSplitTotal: Css = Css("partner-split-total")
  val PartnerSplitData: Css  = Css("partner-split-data")
  val PartnerSplitFlag: Css  = Css("partner-split-flag")

  val PartnerSplitsEditorError: Css =
    Css("partner-splits-editor-error") |+| FomanticStyles.ErrorText
  val PartnerSplitsEditorTable: Css = Css("partner-splits-editor-table")

  // Semantic UI form overrides and extensions
  val Grid: Css              = Css("explore-grid")
  val TwoColumnGrid: Css     = Grid |+| Css("explore-two-columns")
  val ThreeColumnGrid: Css   = Grid |+| Css("explore-three-columns")
  val Compact: Css           = Css("explore-compact")
  val ErrorLabel: Css        = Css("explore-error-label")
  val InputErrorTooltip: Css = Css("explore-input-error-tooltip")
  val FlatFormField: Css     = Css("flat-form-field")

  // Clue websocket connection status
  val ConnectionOK: Css      = Css("connection-ok")
  val ConnectionWarning: Css = Css("connection-warning")
  val ConnectionError: Css   = Css("connection-error")

  // WIP
  val WIP: Css        = Css("wip")
  val WIPWarning: Css = Css("wip-warning")

  // The Target tab contents
  val TargetGrid: Css           = Css("target-grid")
  val TargetAladinCell: Css     = Css("target-aladin-cell")
  val TargetSkyplotCell: Css    = Css("target-skyplot-cell")
  val TargetRaDecMinWidth: Css  = Css("target-ra-dec-min-width")
  val SearchForm: Css           = Css("target-search-form")
  val TargetPropertiesForm: Css = Css("target-properties-form")
  val CatalogueForm: Css        = Css("catalogue-form")

  // styles for icons that acts as buttons
  val ButtonIcon: Css       = Css("explore-button-icon")
  val FormSectionLabel: Css = Css("explore-section-label")
  val UnitsLabel: Css       = Css("explore-units-label")

  // Login box
  val LoginBox: Css          = Css("explore-login-box")
  val LoginBoxLayout: Css    = Css("explore-login-box-layout")
  val LoginTitle: Css        = Css("explore-login-title")
  val LoginTitleWrapper: Css = Css("explore-login-title-wrapper")
  val LoginBoxButton: Css    = Css("explore-login-button")
  val OrcidIcon: Css         = Css("explore-login-orcid-icon")
  val LoginOrcidButton: Css  = Css("explore-login-orcid-button")
  val OrcidIconMenu: Css     = Css("explore-orcid-icon-menu")
  val OrcidMenu: Css         = Css("explore-orcid-menu")

  // Labels
  val WarningLabel: Css = Css("explore-warning-label")
  val ErrrorLabel: Css  = Css("explore-error-label")

  // Version with copy icon
  val Version: Css         = Css("version")
  val VersionUncopied: Css = Css("uncopied")
}
