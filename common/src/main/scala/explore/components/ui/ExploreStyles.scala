// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.ui

import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import react.common.implicits._
import react.common.style._

object ExploreStyles {
  val FadeIn: Css = Css("fade-in")

  val HideReusability = Css("hide-reusability")

  val Tile: Css               = Css("explore-tile")
  val TileTitle: Css          = Css("explore-tile-title")
  val TileTitleMenu: Css      = Css("explore-tile-title-menu")
  val TileTitleStrip: Css     = Css("explore-tile-title-strip")
  val FixedSizeTileTitle: Css = Css("explore-fixed-size-tile-title")
  val TileBackButton: Css     = Css("tile-back-button")
  val TileBody: Css           = Css("explore-tile-body")
  val TileButton: Css         = Css("explore-tile-button")
  val TileControl: Css        = Css("explore-tile-control")
  val TileStateButton: Css    = Css("explore-tile-state-button")
  val TileXSW: Css            = Css("tile-xs-width")
  val TileSMW: Css            = Css("tile-sm-width")
  val TileMDW: Css            = Css("tile-md-width")
  val TileLGW: Css            = Css("tile-lg-width")
  val TileXLW: Css            = Css("tile-xl-width")
  val TileXSH: Css            = Css("tile-xs-height")
  val TileSMH: Css            = Css("tile-sm-height")
  val TileMDH: Css            = Css("tile-md-height")

  val Accented: Css   = Css("explore-accented")
  val TextPlain: Css  = Css("explore-text-plain")
  val TextNoWrap: Css = Css("explore-text-nowrap")

  val FlexContainer: Css = Css("explore-flex-container")
  val FlexWrap: Css      = Css("explore-flex-wrap")

  def FlexGrow(i: Int Refined Interval.Closed[0, 4]): Css   = Css(s"explore-grow-$i")
  def FlexShrink(i: Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-shrink-$i")

  // Move an element to the end of the flex container
  val FlexEnd: Css = Css("explore-flex-end")

  def Grow(i: Int Refined Interval.Closed[1, 4]): Css        = Css(s"explore-grow-$i")
  def ColumnSpan(i: Int Refined Interval.Closed[1, 16]): Css = Css(s"explore-column-span-$i")

  // Change styles for a readonly input
  val StaticData: Css = Css("static-data")

  // re-evaluate the need/use of this as part of overall
  // styling and layout
  val ProposalTile: Css = Css("proposal-tile")

  val MainGrid: Css              = Css("main-grid")
  val MainHeader: Css            = Css("main-header")
  val MainBody: Css              = Css("main-body")
  val MainTitle: Css             = Css("main-title")
  val SizeDetector: Css          = Css("size-detector")
  val ConnectionIcon: Css        = Css("connection-icon")
  val MainUserName: Css          = Css("main-user-name")
  val MainMenu: Css              = Css("main-menu")
  val MainMenuDropdown: Css      = Css("main-menu-dropdown")
  val SideTabs: Css              = Css("sidetabs")
  val SideButton: Css            = Css("side-button")
  val TabSelector: Css           = Css("bottom-tab-selector")
  val BlendedButton: Css         = Css("blended-button")
  val JustifyRight: Css          = Css("justify-right")
  val ObsGroupTitle: Css         = Css("obs-group-title")
  val ObsGroupTitleWithWrap: Css = Css("obs-group-title-with-wrap")
  val DeleteButton: Css          = Css("delete-button")

  val ResizeHandle: Css         = Css("resize-handle")
  val ResizableSeparator: Css   = Css("resize-separator")
  val Tree: Css                 = Css("tree")
  val ResizableSinglePanel: Css = Css("resizable-single-panel")
  val ResizableMultiPanel: Css  = Css("resizable-multi-panel")
  val TreeBody: Css             = Css("tree-body")
  val TreeRGL: Css              = Css("tree-rgl")
  val RGLArea: Css              = Css("rgl-area")
  val RGLPlaceholder: Css       = Css("rgl-placeholder")
  val SinglePanelTile: Css      = Css("single-panel-tile")

  val SideTabsVertical: Css     = Css("sidetabs-body-vertical")
  val SideTabsHorizontal: Css   = Css("sidetabs-body-horizontal")
  val SideTabsDivider: Css      = Css("sidetabs-divider")
  val VerticalButton: Css       = Css("vertical-button")
  val RotationWrapperOuter: Css = Css("rotation-wrapper-outer")
  val RotationWrapperInner: Css = Css("rotation-wrapper-inner")

  val HVCenter: Css        = Css("horizontal-vertical-center")
  val FullHeightWidth: Css = Css("full-height-width")

  val ObsTree: Css                = Css("obs-tree")
  val ObsTreeWrapper: Css         = Css("obs-tree-wrapper")
  val ObsScrollTree: Css          = Css("obs-scroll-tree")
  val ObsUnassigned: Css          = Css("obs-unassigned")
  val ObsTreeGroup: Css           = Css("obs-tree-group")
  val ObsTreeHeader: Css          = Css("obs-tree-header")
  val ObsTreeSection: Css         = Css("obs-tree-section")
  val ObsCount: Css               = Css("obs-count")
  val SelectedObsTreeGroup: Css   = Css("selected-obs-tree-group")
  val UnselectedObsTreeGroup: Css = Css("unselected-obs-tree-group")
  val ObsTreeGroupHeader: Css     = Css("obs-tree-group-header")
  val ObsTreeItem: Css            = Css("obs-tree-item")
  val TreeToolbar: Css            = Css("tree-toolbar")
  val ObsBadge: Css               = Css("obs-badge")
  val ObsBadgeSubtitle: Css       = Css("obs-badge-subtitle")
  val ObsBadgeSubtitleInput: Css  = Css("obs-badge-subtitle-input")
  val ObsBadgeSubtitleAdd: Css    = Css("obs-badge-subtitle-add")
  val ObsBadgeSubtitleEdit: Css   = Css("obs-badge-subtitle-edit")
  val ObsBadgeSubtitleDelete: Css = Css("obs-badge-subtitle-delete")
  val ObsBadgeId: Css             = Css("obs-badge-id")
  val ObsBadgeTargetAndId: Css    = Css("obs-badge-target-and-id")
  val ObsBadgeHeader: Css         = Css("obs-badge-header")
  val ObsBadgeDescription: Css    = Css("obs-badge-description")
  val ObsBadgeExtra: Css          = Css("obs-badge-extra")
  val ObsDeleteButton: Css        = Css("obs-delete-button")
  val ObsStatusSelect: Css        = Css("obs-status-select")
  val ObsActiveStatusToggle: Css  = Css("obs-active-status-toggle")
  val SelectedObsItem: Css        = Css("selected-obs-item")
  val ObsItem: Css                = Css("obs-item")
  val TrashIcon: Css              = Css("trash-icon")
  val EraserIcon: Css             = Css("eraser-icon")

  val ProgramsPopup     = Css("programs-popup")
  val ProgramTable      = Css("program-table")
  val ProgramAdd        = Css("program-add")
  val ProgramName       = Css("program-name")
  val ProgramNameInput  = Css("program-name-input")
  val ProgramNameEdit   = Css("program-name-edit")
  val ProgramNameDelete = Css("program-name-delete")

  val ObserverNotes: Css = Css("observer-notes")
  val NotesWrapper: Css  = Css("observer-notes-wrapper")

  val DraggingOver: Css = Css("dragging-over")

  val TargetTileBody: Css            = Css("target-tile-body")
  val TargetTileEditor: Css          = Css("target-tile-editor")
  val TargetSourceProfileEditor: Css = Css("target-source-profile-editor")
  val Gaussian: Css                  = Css("gaussian")

  val ElevationPlotTile: Css               = Css("elevation-plot-tile")
  val ElevationPlotTileBody: Css           = Css("elevation-plot-tile-body")
  val ElevationPlot: Css                   = Css("elevation-plot")
  val ElevationPlotSection: Css            = Css("elevation-plot-section")
  val ElevationPlotControls: Css           = Css("elevation-plot-controls")
  val ElevationPlotDatePickerControls: Css = Css("elevation-plot-datepicker-controls")
  val ElevationPlotDatePicker: Css         = Css("ui input elevation-plot-datepicker")
  val ElevationPlotDateButton: Css         = Css("elevation-plot-date-button")
  val PlotToggleCheckbox: Css              = Css("plot-toggle-checkbox")
  val PlotToggle: Css                      = Css("plot-toggle")

  val AladinContainerColumn: Css  = Css("aladin-container-column")
  val AladinContainerBody: Css    = Css("aladin-container-body")
  val AladinFOV: Css              = Css("aladin-status-fov")
  val AladinGuideStarLoading: Css = Css("aladin-status-gs-loading")
  val AladinGuideStar: Css        = Css("aladin-status-gs")
  val AladinDetailText: Css       = Css("aladin-detail-text")
  val AladinCurrentCoords: Css    = Css("aladin-status-current-coordinates")
  val AladinCenterButton: Css     = Css("aladin-status-center-button")
  val AladinSearchIcon: Css       = Css("aladin-search-icon")

  val AgsOverlay: Css          = Css("ags-overlay")
  val AgsDescription: Css      = Css("ags-description")
  val AgsGuideSpeed: Css       = Css("ags-guide-speed")
  val AgsGoodIQ: Css           = Css("ags-good-iq-color")
  val AgsGBrightness: Css      = Css("ags-g-brightness")
  val AgsCoordinates: Css      = Css("ags-coordinates")
  val AgsNavigationButton: Css = Css("ags-navigation-button")
  val AgsNavigation: Css       = Css("ags-navigation")

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
  val Grid: Css                   = Css("explore-grid")
  val TwoColumnGrid: Css          = Grid |+| Css("explore-two-columns")
  val ThreeColumnGrid: Css        = Grid |+| Css("explore-three-columns")
  val Compact: Css                = Css("explore-compact")
  val ErrorLabel: Css             = Css("explore-error-label")
  val InputErrorTooltip: Css      = Css("explore-input-error-tooltip")
  val InputErrorTooltipBelow: Css = Css("explore-input-error-tooltip-below")
  val FlatFormField: Css          = Css("flat-form-field")

  // Clue websocket connection status
  val ConnectionOK: Css      = Css("connection-ok")
  val ConnectionWarning: Css = Css("connection-warning")
  val ConnectionError: Css   = Css("connection-error")

  // WIP
  val WIP: Css        = Css("wip")
  val WIPWarning: Css = Css("wip-warning")

  // Edit Warning
  val EditWarning: Css = Css("edit-warning")

  // The Observation tab contents
  val ObservationTiles: Css = Css("observation-tiles")
  val ConstraintsTile: Css  = Css("constraints-tile")

  // The Target tab contents
  val TargetGrid: Css     = Css("target-grid")
  val AsterismEditor: Css = Css("target-asterism-editor")
  val AsterismTable: Css  = Css("target-asterism-table")
  val BrightnessCell: Css = Css("target-brightness-cell")

  val TargetAladinCell: Css       = Css("target-aladin-cell")
  val TargetAladin: Css           = Css("aladin-target")
  val TargetRaDecMinWidth: Css    = Css("target-ra-dec-min-width")
  val TargetForm: Css             = Css("target-form")
  val TargetProperMotionForm: Css = Css("target-proper-motion-form")
  val SearchForm: Css             = Css("target-search-form")
  val TargetRVControls: Css       = Css("target-rv-controls")
  val CatalogueForm: Css          = Css("catalogue-form")

  // The Constraints tab contents
  val ConstraintsGrid                = Css("constraints-grid")
  val ConstraintsNameField           = Css("constraints-name-field")
  val ConstraintsElevationRangeGroup = Css("constraints-elevation-range")
  val ConstraintsTileSelector        = Css("constraints-tile-selector")
  val ElevationRangePicker           = Css("elevation-range-picker")
  val ElevationRangeEntry            = Css("elevation-range-entry")

  // styles for icons that acts as buttons
  val ButtonIcon: Css       = Css("explore-button-icon")
  val FormSectionLabel: Css = Css("explore-section-label")
  val UnitsLabel: Css       = Css("explore-units-label")
  val UnitsTableLabel: Css  = Css("explore-table-units-label")

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
  val WarningIcon: Css  = Css("explore-warning-icon")
  val WarningInput: Css = Css("explore-warning-input")
  val ErrorIcon: Css    = Css("explore-error-icon")

  // Version with copy icon
  val Version: Css         = Css("version")
  val VersionUncopied: Css = Css("uncopied")

  val BrightnessesTableWrapper: Css            = Css("explore-brightnesses-wrapper")
  val BrightnessesTableContainer: Css          = Css("explore-brightnesses-container")
  val BrightnessesTableFooter: Css             = Css("explore-brightnesses-footer")
  val BrightnessesTableUnitsDropdown: Css      = Css("explore-brightnesses-units-dropdown")
  val BrightnessesTableDeletButtonWrapper: Css = Css("explore-brightnesses-delete-button-wrapper")
  val BrightnessAddButton: Css                 = Css("explore-brightnesses-add-button")
  val EmptyTreeContent: Css                    = Css("explore-empty-tree-content")

  // This is rendered without React, so we include SUI classes.
  val CrashMessage: Css = Css(List("ui", "large", "label", "crash-message"))

  // Help styles
  val HelpIcon: Css         = Css("explore-help-icon")
  val HelpIconFloating: Css = Css("explore-help-icon-floating")
  val HelpSidebar: Css      = Css("explore-help-sidebar")
  val HelpTitle: Css        = Css("explore-help-title")
  val HelpTitleLabel: Css   = Css("explore-help-title-label")
  val HelpBody: Css         = Css("explore-help-body")
  val HelpMarkdownBody: Css = Css("markdown-body")

  val StepTableHeader: Css = Css("step-table-header")
  val StepGuided: Css      = Css("step-guided")

  val ExploreForm: Css          = Css("explore-form")
  val SkipToNext: Css           = Css("explore-skip-to-next")
  val SeqGenParametersForm: Css = Css("seq-gen-parameters-form")

  // Configuration tile
  val ObsConfigurationForm: Css         = Css("explore-obs-configuration-form")
  val ObsInstantTileTitle: Css          = Css("explore-obs-instant-tile-title")
  val ObsConfigurationObsPA: Css        = Css("explore-obs-configuration-pa")
  val ObsConfigurationObsTime: Css      = Css("explore-obs-configuration-time")
  val ConfigurationGrid: Css            = Css("explore-configuration-grid")
  val BasicConfigurationGrid: Css       = Css("explore-basic-configuration-grid")
  val BasicConfigurationForm: Css       = Css("explore-basic-configuration-form")
  val BasicConfigurationButtons: Css    = Css("explore-basic-configuration-buttons")
  val AdvancedConfiguration: Css        = Css("explore-advanced-configuration")
  val AdvancedConfigurationGrid: Css    = Css("explore-advanced-configuration-grid")
  val AdvancedConfigurationCol1: Css    = Css("explore-advanced-configuration-col1")
  val AdvancedConfigurationCol2: Css    = Css("explore-advanced-configuration-col2")
  val AdvancedConfigurationCol3: Css    = Css("explore-advanced-configuration-col3")
  val AdvancedConfigurationButtons: Css = Css("explore-advanced-configuration-buttons")
  val ConfigurationCapabilities: Css    = Css("explore-configuration-capabilities")
  val InputWithLabel: Css               = Css("explore-configuration-input-with-units")
  val ConfigurationFilter: Css          = Css("explore-configuration-filter")
  val ConfigurationFilterItem: Css      = Css("explore-configuration-filter-item")
  val ExploreTable: Css                 = Css("explore-table")
  val Virtualized: Css                  = Css("virtualized")
  val TableRowSelected: Css             = Css("explore-table-row-selected")
  val ModesTableTitle: Css              = Css("explore-modes-table-title")
  val ModesTable: Css                   = Css("explore-modes-table")
  val ModesHeader: Css                  = Css("explore-modes-header")
  val ScrollButton: Css                 = Css("scroll-button")
  val SelectedUp: Css                   = Css("selected-up")
  val SelectedDown: Css                 = Css("selected-down")
  val ITCHeaderCell: Css                = Css("explore-modes-table-itc-header-cell")
  val ITCCell: Css                      = Css("explore-modes-table-itc-cell")
  val SequenceObsSutitle                = Css("explore-sequence-obs-subtitle")

  val ButtonCopy: Css         = Css("explore-button-copy")
  val ButtonsUndo: Css        = Css("explore-buttons-undo")
  val ButtonsUndoLabel: Css   = Css("explore-buttons-undo-label")
  val ButtonSummary: Css      = Css("explore-button-summary")
  val TitleUndoButtons: Css   = Css("explore-title-undo-buttons")
  val TitleSelectColumns: Css = Css("explore-title-select-columns")
  val SelectColumns: Css      = Css("explore-select-columns")

  val VeryCompact: Css = Css("verycompact")

  val SharedEditWarning: Css = Css("explore-shared-edit-warning")

  val StickyColumn: Css            = Css("sticky-column")
  val StickyHeader: Css            = Css("sticky-header")
  val TargetSummaryDelete: Css     = Css("explore-target-summary-delete")
  val TargetSummarySelect: Css     = Css("explore-target-summary-select")
  val TargetSummaryId: Css         = Css("explore-target-summary-id")
  val TargetSummaryType: Css       = Css("explore-target-summary-type")
  val TargetSummaryName: Css       = Css("explore-target-summary-name")
  val WithDelete: Css              = Css("with-delete")
  val WithExpander: Css            = Css("with-expander")
  val WithSelect: Css              = Css("with-select")
  val WithId: Css                  = Css("with-id")
  val TargetSummarySubRowCell: Css = Css("explore-target-summary-subrow-cell")
  val ConstraintsSummaryEdit: Css  = Css("explore-constraints-summary-edit")

  val ModalCloseButton: Css     = Css("modal-close-button")
  val UserSelectionButtons: Css = Css("user-selection-buttons")
  val PaddedRightIcon: Css      = Css("padded-right-icon")

  val Table: Css   = Css("tble")
  val THead: Css   = Css("thead")
  val TBody: Css   = Css("tbody")
  val TFoot: Css   = Css("tfoot")
  val TR: Css      = Css("tr")
  val TH: Css      = Css("th")
  val TD: Css      = Css("td")
  val EvenRow: Css = Css("even-row")
  val OddRow: Css  = Css("odd-row")

  // Target Search Popup
  val TargetSearchTop: Css           = Css("explore-target-search-top")
  val TargetSearchInput: Css         = Css("explore-target-search-input")
  val TargetSearchPreview: Css       = Css("explore-target-search-preview")
  val TargetSearchAladin: Css        = Css("aladin-search-target")
  val TargetSearchResults: Css       = Css("explore-target-search-results")
  val TargetSearchResultsSource: Css = Css("explore-target-search-results-source")

  val NewEmissionLineWavelength: Css = Css("explore-new-emissionline-wavelength")

  // Aladin Target classes
  val ScienceTarget: Css           = Css("science-target")
  val CircleTarget: Css            = Css("circle-target")
  val BaseTarget                   = Css("base-target")
  val CrosshairTarget              = Css("crosshair-target")
  val ArrowBetweenTargets          = Css("arrow-between-targets")
  val GuideStarCandidateTarget     = Css("guide-star-candidate-target")
  val GuideStarCandidateCrowded    = Css("guide-star-candidate-target-crowded")
  val GuideStarTarget              = Css("guide-star-target")
  val GuideStarTargetReachable     = Css("guide-star-candidate-target-reachable")
  val GuideStarCandidateTargetBase = Css("guide-star-candidate-target-base")
  val GuideStarCandidateVisible    = Css("guide-star-candidate-target-visible")
  val PMCorrectionLine             = Css("proper-motion-line")
  val PMGSCorrectionLine           = Css("proper-motion-line-gs")
  val AladinToolbox                = Css("explore-aladin-toolbox-button")
  val AladinZoomControl            = Css("explore-aladin-zoom-control")
  val ButtonOnAladin               = Css("explore-aladin-button")
  val AladinSettingsMenu           = Css("explore-aladin-settings-menu")
  val BlendedSVG                   = Css("blended-svg")

  val ExploreToast     = Css("explore-toast")
  val ExploreToastGrid = Css("explore-toast-grid")

  // For icons that will be show instead of an input during some circumstances.
  val InputReplacementIcon: Css = Css("input-replacement-icon") |+| Css("field")

  // For making a Input have a "X" icon for clearing, like the dropdowns do.
  // See `clearInputIcon` in `explore.utils.package`
  val ClearableInputIcon: Css         = Css("clearable-input-icon")
  val ClearableInputPaddingReset: Css = Css("clearable-input-padding-reset")
}
