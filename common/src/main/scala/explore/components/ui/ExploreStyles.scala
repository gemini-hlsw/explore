// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.ui

import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import lucuma.react.common.style.*

object ExploreStyles:
  val FadeIn: Css = Css("fade-in")

  val HideReusability = Css("hide-reusability")

  val GlobalErrorDialog = Css("explore-global-error-dialog")

  val Tile: Css                        = Css("explore-tile")
  val TileTitle: Css                   = Css("explore-tile-title")
  val TileTitleText: Css               = Css("explore-tile-title-text")
  val TileTitleMenu: Css               = Css("explore-tile-title-menu")
  val TileDraggable: Css               = Css("explore-tile-draggable")
  val TileTitleConstraintSelector: Css = Css("explore-tile-title-constraint-selector")
  val TileTitleConfigSelector: Css     = Css("explore-tile-title-config-selector")
  val TileControlButtons: Css          = Css("explore-tile-control-buttons")
  val TileBackButton: Css              = Css("tile-back-button")
  val TileBody: Css                    = Css("explore-tile-body")
  val TileButton: Css                  = Css("explore-tile-button")
  val TileStateButton: Css             = Css("explore-tile-state-button")

  val Accented: Css    = Css("explore-accented")
  val TextPlain: Css   = Css("explore-text-plain")
  val UpgradeLink: Css = Css("explore-upgrade-link")

  val FlexContainer: Css = Css("explore-flex-container")
  val FlexWrap: Css      = Css("explore-flex-wrap")

  def FlexGrow(i:   Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-grow-$i")
  def FlexShrink(i: Int Refined Interval.Closed[0, 4]): Css = Css(s"explore-shrink-$i")

  // Move an element to the end of the flex container
  val FlexEnd: Css = Css("explore-flex-end")

  def Grow(i:       Int Refined Interval.Closed[1, 4]): Css  = Css(s"explore-grow-$i")
  def ColumnSpan(i: Int Refined Interval.Closed[1, 16]): Css = Css(s"explore-column-span-$i")

  // Change styles for a readonly input
  val StaticData: Css = Css("static-data")

  val ConnectionIcon: Css        = Css("connection-icon")
  val ObsGroupTitle: Css         = Css("obs-group-title")
  val ObsGroupTitleWithWrap: Css = Css("obs-group-title-with-wrap")
  val ObsGroupTitleWithList: Css = Css("obs-group-title-with-list")
  val DeleteButton: Css          = Css("delete-button")

  val MainTitleProgramId: Css = Css("main-title-program-id")
  val ResizeHandle: Css       = Css("resize-handle")
  val ResizableSeparator: Css = Css("resize-separator")
  val Tree: Css               = Css("tree")
  val TreeBody: Css           = Css("tree-body")
  val TreeRGL: Css            = Css("tree-rgl")
  val SinglePanelTile: Css    = Css("single-panel-tile")
  val MultiPanelTile: Css     = Css("multi-panel-tile")

  val FullHeightWidth: Css = Css("full-height-width")

  val ObsTree: Css                     = Css("obs-tree")
  val ObsTreeHideShow: Css             = Css("obs-hide-show-button")
  val ObsHiddenToolbar: Css            = Css("obs-tree-hidden-toolbar")
  val ObsTreeWrapper: Css              = Css("obs-tree-wrapper")
  val ObsScrollTree: Css               = Css("obs-scroll-tree")
  val ObsUnassigned: Css               = Css("obs-unassigned")
  val ObsTreeGroup: Css                = Css("obs-tree-group")
  val ObsTreeHeader: Css               = Css("obs-tree-header")
  val ObsTreeSection: Css              = Css("obs-tree-section")
  val ObsCount: Css                    = Css("obs-count")
  val SelectedObsTreeGroup: Css        = Css("selected-obs-tree-group")
  val UnselectedObsTreeGroup: Css      = Css("unselected-obs-tree-group")
  val ObsTreeGroupHeader: Css          = Css("obs-tree-group-header")
  val GroupBadgeLeft: Css              = Css("group-badge-left")
  val GroupBadgeRight: Css             = Css("group-badge-right")
  val ObsTreeItem: Css                 = Css("obs-tree-item")
  val ObsTreeGroupLeaf: Css            = Css("obs-tree-group-leaf")
  val TreeToolbar: Css                 = Css("tree-toolbar")
  val ObsBadge: Css                    = Css("obs-badge")
  val ObsBadgeSubtitle: Css            = Css("obs-badge-subtitle")
  val ObsBadgeSubtitleInput: Css       = Css("obs-badge-subtitle-input")
  val ObsBadgeSubtitleAdd: Css         = Css("obs-badge-subtitle-add")
  val ObsBadgeSubtitleEdit: Css        = Css("obs-badge-subtitle-edit")
  val ObsBadgeSubtitleDelete: Css      = Css("obs-badge-subtitle-delete")
  val ObsBadgeId: Css                  = Css("obs-badge-id")
  val ObsBadgeTargetAndId: Css         = Css("obs-badge-target-and-id")
  val ObsBadgeHeader: Css              = Css("obs-badge-header")
  val ObsBadgeMeta: Css                = Css("obs-badge-meta")
  val ObsBadgeDescription: Css         = Css("obs-badge-description")
  val ObsBadgeExtra: Css               = Css("obs-badge-extra")
  val ObsBadgeSelected: Css            = Css("obs-badge-selected")
  val ObsDeleteButton: Css             = Css("obs-delete-button")
  val ObsCloneButton: Css              = Css("obs-clone-button")
  val ObsScienceBandButton: Css        = Css("obs-science-band-button")
  val ObsStateSelect: Css              = Css("obs-state-select")
  val ObsStateSelectWrapper: Css       = Css("obs-state-select-wrapper")
  val ObsStateSelectPanel: Css         = Css("obs-state-select-panel")
  val ObservationsSummaryTable: Css    = Css("observations-summary-table")
  val ObservationsSummaryIndexCol: Css = Css("observations-summary-index-col")
  val ObservationsSummaryAdd: Css      = Css("observations-summary-add")
  val SelectedObsItem: Css             = Css("selected-obs-item")
  val SelectedGroupItem: Css           = Css("selected-group-item")
  val ObsItem: Css                     = Css("obs-item")
  val TrashIcon: Css                   = Css("trash-icon")
  val EraserIcon: Css                  = Css("eraser-icon")

  val ScienceBandPopupMenu = Css("science-band-popup-menu")

  val GroupEditTile: Css       = Css("group-edit-tile")
  val GroupEditTitle: Css      = Css("group-edit-title")
  val GroupEditTitleTimes: Css = Css("group-edit-title-times")
  val GroupTypeSelect: Css     = Css("group-type-select")
  val GroupChangeButtons: Css  = Css("group-change-buttons")
  val GroupEditNote: Css       = Css("group-edit-note")
  val GroupForm: Css           = Css("group-form")
  val GroupDelaysForm: Css     = Css("group-delays-form")
  val GroupPlannedTime: Css    = Css("group-planned-time")
  val GroupWarnings: Css       = Css("group-warnings")

  val ApiKeysPopup          = Css("api-keys-popup")
  val ApiKeysTable          = Css("api-keys-table")
  val ApiKeyDelete          = Css("api-keys-delete")
  val ApiKeysTableMod       = Css("api-keys-table-mod")
  val NewApiKey             = Css("new-api-key")
  val NewApiKeyLabel        = Css("new-api-key-label")
  val UserPreferencesFooter = Css("user-preferences-footer")
  val UserPreferencesNewKey = Css("user-preferences-new-key")
  val EmptyUserPreferences  = Css("empty-user-preferences")
  val WavelengthUnits       = Css("wavelength-units")

  val ProgramsPopup    = Css("programs-popup")
  val ProgramTable     = Css("program-table")
  val ProgramName      = Css("program-name")
  val ProgramNameInput = Css("program-name-input")

  val NotesTile: Css = Css("observer-notes-tile")

  val FinderChartsTile: Css          = Css("finder-charts-tile")
  val FinderChartsBody: Css          = Css("finder-charts-body")
  val FinderChartsTileTitle: Css     = Css("finder-charts-tile-title")
  val FinderChartsImage: Css         = Css("finder-charts-image")
  val FinderChartsImageInverted: Css = Css("finder-charts-image-inverted")
  val FinderChartsTools: Css         = Css("finder-charts-tools")
  val FinderChartsAttachments: Css   = Css("finder-charts-attachments")
  val FinderChartsButton: Css        = Css("finder-charts-button")
  val FinderChartsButtonPA: Css      = Css("finder-charts-button-pa")
  val FinderChartsTable: Css         = Css("finder-charts-table")
  val FinderChartsTableDisabled: Css = Css("finder-charts-table-disabled")
  val FinderChartsTableHeader: Css   = Css("finder-charts-table-header")
  val FinderChartsFileName: Css      = Css("finder-charts-table-filename")
  val FinderChartsLoadProgress: Css  = Css("finder-charts-table-progress")
  val FinderChartsBackground: Css    = Css("finder-charts-background")

  val FinderChartsSelectorSection: Css = Css("finder-charts-selector-section")

  val DraggingOver: Css = Css("dragging-over")

  val TargetTileController: Css      = Css("target-tile-controller")
  val TargetTileBody: Css            = Css("target-tile-body")
  val TargetTileEditor: Css          = Css("target-tile-editor")
  val TargetTileObsDuration: Css     = Css("target-tile-obs-duration")
  val TargetObsDefaultDuration: Css  = Css("target-tile-obs-default-duration")
  val TargetTileObsUTC: Css          = Css("target-tile-obs-utc")
  val AddTargetButton: Css           = Css("add-target-button")
  val TargetSourceProfileEditor: Css = Css("target-source-profile-editor")
  val WithGaussian: Css              = Css("with-gaussian")
  val WithCatalogInfo: Css           = Css("with-catalog-info")

  val ElevationPlotTileBody: Css           = Css("elevation-plot-tile-body")
  val ElevationPlot: Css                   = Css("elevation-plot")
  val ElevationPlotControls: Css           = Css("elevation-plot-controls")
  val ElevationPlotDatePickerControls: Css = Css("elevation-plot-datepicker-controls")
  val ElevationPlotDateInput: Css          = Css("elevation-plot-date-input")
  val ElevationPlotDateButton: Css         = Css("elevation-plot-date-button")

  val ItcPlotSelector: Css        = Css("itc-plot-selector")
  val ItcPlotSection: Css         = Css("itc-plot-section")
  val ItcPlotDetailsHidden: Css   = Css("itc-plot-details-hidden")
  val ItcPlotHelpIcon: Css        = Css("itc-plot-help-icon")
  val ItcPlotDetailsToggle: Css   = Css("itc-plot-details-toggle")
  val ItcPlotChart: Css           = Css("itc-plot-chart")
  val ItcPlotWvPlotLine: Css      = Css("itc-plot-wavelength-line")
  val ItcPlotDescription: Css     = Css("itc-plot-description")
  val ItcPlotTileBody: Css        = Css("itc-plot-tile-body")
  val ItcPlotBody: Css            = Css("itc-plot-body")
  val ItcPlotControls: Css        = Css("itc-plot-controls")
  val ItcTileBody: Css            = Css("explore-itc-tile-body")
  val ItcTileTitle: Css           = Css("explore-itc-tile-title")
  val ItcTileTargetSelector: Css  = Css("explore-itc-tile-target-selector")
  val ItcErrorIcon: Css           = Css("itc-error-icon")
  val ItcSourceTooBrightIcon: Css = Css("itc-source-too-bright-icon")

  val AladinContainerColumn: Css  = Css("aladin-container-column")
  val AladinContainerBody: Css    = Css("aladin-container-body")
  val AladinFOV: Css              = Css("aladin-status-fov")
  val AladinGuideStarLoading: Css = Css("aladin-status-gs-loading")
  val AladinGuideStar: Css        = Css("aladin-status-gs")
  val AladinDetailText: Css       = Css("aladin-detail-text")
  val AladinCurrentCoords: Css    = Css("aladin-status-current-coordinates")
  val AladinCenterButton: Css     = Css("aladin-status-center-button")
  val AladinSearchIcon: Css       = Css("aladin-search-icon")
  val AladinFullScreen: Css       = Css("aladin-full-screen")
  val AladinRangeControl: Css     = Css("aladin-range-control")

  val VisualizationTooltip: Css       = Css("visualization-tooltip")
  val VisualizationTooltipTarget: Css = Css("visualization-tooltip-target")

  val AgsOverlay: Css          = Css("ags-overlay")
  val AgsDescription: Css      = Css("ags-description")
  val AgsGuideSpeed: Css       = Css("ags-guide-speed")
  val AgsFast: Css             = Css("ags-fast-color")
  val AgsNotFound: Css         = Css("ags-not-found")
  val AgsMedium: Css           = Css("ags-medium-color")
  val AgsSlow: Css             = Css("ags-slow-color")
  val AgsTooltip: Css          = Css("ags-tooltip")
  val AgsGBrightness: Css      = Css("ags-g-brightness")
  val AgsCoordinates: Css      = Css("ags-coordinates")
  val AgsNavigationButton: Css = Css("ags-navigation-button")
  val AgsNavigation: Css       = Css("ags-navigation")

  val ProposalTab: Css             = Css("explore-proposal-tab")
  val ProposalDetailsGrid: Css     = Css("explore-proposal-details-grid")
  val ProposalAbstract: Css        = Css("explore-proposal-abstract")
  val AbstractTitleTooLong: Css    = Css("explore-abstract-too-long")
  val ProposalSubmissionBar: Css   = Css("explore-proposal-submission-line")
  val ProposalDeadline: Css        = Css("explore-proposal-deadline")
  val ProposalAttachmentsTile: Css = Css("explore-proposal-attachments-tile")
  val ProposalErrorsTile: Css      = Css("explore-proposal-errors-tile")
  val CfpData: Css                 = Css("cfp-data")

  val ProgramDescription: Css = Css("explore-program-description")

  val PartnerSplitsMissing: Css        = Css("partner-splits-missing")
  val PartnerSplitsGrid: Css           = Css("partner-splits-grid")
  val PartnerSplitsGridMinPctItem: Css = Css("partner-splits-grid-min-pct-item")
  val PartnerSplitsGridMinPct: Css     = Css("partner-splits-grid-min-pct")
  val PartnerSplitsGridTotal: Css      = Css("partner-splits-grid-total")
  val PartnerSplitData: Css            = Css("partner-split-data")
  val PartnerSplitFlag: Css            = Css("partner-split-flag")
  val PartnerSelector: Css             = Css("partner-selector")
  val PartnerFlagItem: Css             = Css("partner-flag-item")

  val PartnerSplitsEditorDialog: Css = Css("partner-splits-editor-dialog")
  val CompactOverlayPanel: Css       = Css("compact-overlay-panel")

  // Program user create/invite
  val AddProgramUserButton: Css = Css("add-program-user-button")
  val InviteUserPopup: Css      = Css("invite-user-popup")

  // Semantic UI form overrides and extensions
  val Grid: Css                   = Css("explore-grid")
  val Compact: Css                = Css("explore-compact")
  val ErrorLabel: Css             = Css("explore-error-label")
  val InputErrorTooltip: Css      = Css("explore-input-error-tooltip")
  val InputErrorTooltipBelow: Css = Css("explore-input-error-tooltip-below")
  val FlatFormField: Css          = Css("flat-form-field")

  // WIP
  val WIP: Css        = Css("wip")
  val WIPWarning: Css = Css("wip-warning")

  // Edit Warning
  val EditWarning: Css = Css("edit-warning")

  // The Target tab contents
  val TargetGrid: Css     = Css("target-grid")
  val AsterismEditor: Css = Css("target-asterism-editor")
  val AsterismTable: Css  = Css("target-asterism-table")
  val BrightnessCell: Css = Css("target-brightness-cell")

  val AsterismEditorTileTitle: Css = Css("asterism-editor-tile-title")

  val TargetAladinCell: Css          = Css("target-aladin-cell")
  val TargetAladin: Css              = Css("aladin-target")
  val AladinHelpIcon: Css            = Css("aladin-help-icon")
  val AladinSurvey: Css              = Css("aladin-survey")
  val TargetAladinDisableMouse: Css  = Css("aladin-target-disable-mouse")
  val TargetForm: Css                = Css("target-form")
  val TargetProperMotionForm: Css    = Css("target-proper-motion-form")
  val TargetRVControls: Css          = Css("target-rv-controls")
  val CatalogueForm: Css             = Css("catalogue-form")
  val SEDTypeDropdown: Css           = Css("target-sed-type-dropdown")
  // The Constraints tab contents
  val ConstraintsGrid                = Css("constraints-grid")
  val ConstraintsNameField           = Css("constraints-name-field")
  val ConstraintsElevationRangeGroup = Css("constraints-elevation-range")
  val ConstraintsLikelihood          = Css("constraints-likelihood")
  val ConstraintsSetLikelihood       = Css("constraints-set-likelihood")
  val ElevationRangePicker           = Css("elevation-range-picker")
  val ElevationRangeEntry            = Css("elevation-range-entry")

  // styles for icons that acts as buttons
  val ButtonIcon: Css       = Css("explore-button-icon")
  val FormSectionLabel: Css = Css("explore-section-label")
  val UnitsTableLabel: Css  = Css("explore-table-units-label")

  val LoginTitle: Css    = Css("explore-login-title")
  val OrcidIconMenu: Css = Css("explore-orcid-icon-menu")

  // Labels
  val WarningLabel: Css       = Css("explore-warning-label")
  val WarningIcon: Css        = Css("explore-warning-icon")
  val WarningInput: Css       = Css("explore-warning-input")
  val ErrorIcon: Css          = Css("explore-error-icon")
  val SuccessIcon: Css        = Css("explore-success-icon")
  val IndentLabel: Css        = Css("explore-indent-label")
  val RequiredForItcText: Css = Css("required-for-itc-text")

  val BrightnessesTableWrapper: Css            = Css("explore-brightnesses-wrapper")
  val BrightnessesContainer: Css               = Css("explore-brightnesses-container")
  val BrightnessesTableFooter: Css             = Css("explore-brightnesses-footer")
  val BrightnessesTableUnitsDropdown: Css      = Css("explore-brightnesses-units-dropdown")
  val BrightnessesTableDeletButtonWrapper: Css = Css("explore-brightnesses-delete-button-wrapper")
  val EmptyTreeContent: Css                    = Css("explore-empty-tree-content")

  val AttachmentsTable: Css           = Css("explore-attachments-table")
  val AttachmentsTableTypeSelect: Css = Css("explore-attachments-type-select")
  val AttachmentName: Css             = Css("explore-attachment-name")
  val AttachmentNameInput: Css        = Css("explore-attachment-name-input")

  val FileUpload: Css = Css("explore-fileupload")

  // This is rendered without React, so we include SUI classes.
  val CrashMessage: Css = Css(List("ui", "large", "label", "crash-message"))

  // Help styles
  val HelpIcon: Css         = Css("explore-help-icon")
  val HelpIconFloating: Css = Css("explore-help-icon-floating")
  val HelpSidebar: Css      = Css("explore-help-sidebar")
  val HelpTitle: Css        = Css("explore-help-title")
  val HelpBody: Css         = Css("explore-help-body")
  val HelpMarkdownBody: Css = Css("markdown-body")

  val DatePicker: Css           = Css("explore-datepicker")
  val DatePickerTimeEditor: Css = Css("explore-datepicker-time-editor")

  val TargetImportForm: Css        = Css("explore-target-import-form")
  val TargetImportDescription: Css = Css("explore-target-import-description")
  val TargetImportErrors: Css      = Css("explore-target-import-errors")
  val TargetImportDialog: Css      = Css("explore-target-import-dialog")

  // Configuration tile
  val ConfigurationTileBody: Css        = Css("explore-configuration-tile-body")
  val PAConfigurationForm: Css          = Css("explore-pa-configuration-form")
  val PAConfigurationAngle: Css         = Css("explore-pa-configuration-angle")
  val AveragePA: Css                    = Css("explore-average-pa")
  val ObsInstantTileTitle: Css          = Css("explore-obs-instant-tile-title")
  val ConfigurationGrid: Css            = Css("explore-configuration-grid")
  val BasicConfigurationGrid: Css       = Css("explore-basic-configuration-grid")
  val BasicConfigurationForm: Css       = Css("explore-basic-configuration-form")
  val TimeAndCountField: Css            = Css("explore-configuration-time-and-count")
  val BasicConfigurationFocalPlane: Css = Css("explore-basic-configuration-focal-plane")
  val BasicConfigurationButtons: Css    = Css("explore-basic-configuration-buttons")
  val AdvancedConfiguration: Css        = Css("explore-advanced-configuration")
  val AdvancedConfigurationGrid: Css    = Css("explore-advanced-configuration-grid")
  val AdvancedConfigurationCol1: Css    = Css("explore-advanced-configuration-col1")
  val AdvancedConfigurationCol2: Css    = Css("explore-advanced-configuration-col2")
  val AdvancedConfigurationCol3: Css    = Css("explore-advanced-configuration-col3")
  val AdvancedConfigurationButtons: Css = Css("explore-advanced-configuration-buttons")
  val AdvancedConfigurationBinning: Css = Css("explore-advanced-configuration-binning")
  val ConfigurationImagingFilters: Css  = Css("explore-configuration-imaging-filters")
  val ExploreTable: Css                 = Css("explore-table")
  val ExploreTableEmpty: Css            = Css("explore-table-emptymessage")
  val SpectroscopyTableEmpty: Css       = Css("spectroscopy-table-emptymessage")
  val ExploreSelectableTable: Css       = Css("explore-selectable-table")
  val ExploreBorderTable: Css           = Css("explore-border-table")
  val TableRowSelected: Css             = Css("explore-table-row-selected")
  val TableRowSelectedStart: Css        = Css("explore-table-row-selected-start")
  val TableRowSelectedSpan: Css         = Css("explore-table-row-selected-span")
  val TableRowSelectedEnd: Css          = Css("explore-table-row-selected-end")
  val ModesTableTitle: Css              = Css("explore-modes-table-title")
  val ModesTableTarget: Css             = Css("explore-modes-table-target")
  val ModesTableInfo: Css               = Css("explore-modes-table-info")
  val ModesTableCount: Css              = Css("explore-modes-table-count")
  val ModesTable: Css                   = Css("explore-modes-table")
  val ModesHeader: Css                  = Css("explore-modes-header")
  val ScrollButton: Css                 = Css("scroll-button")
  val SelectedUp: Css                   = Css("selected-up")
  val SelectedDown: Css                 = Css("selected-down")
  val ITCHeaderCell: Css                = Css("explore-modes-table-itc-header-cell")
  val ITCCell: Css                      = Css("explore-modes-table-itc-cell")

  // Sequence Viewer
  val SequencesPanel         = Css("explore-sequences-panel")
  val SequencesPanelError    = Css("explore-sequences-panel-error")
  val SequenceRowDone        = Css("explore-sequence-row-done")
  val SequenceRowHeader      = Css("explore-sequence-row-header")
  val SequenceRowFirstInAtom = Css("explore-sequence-row-first-in-atom")
  val SequenceTileController = Css("explore-sequence-tile-controller")
  val SequenceTileTitle      = Css("explore-sequence-tile-title")
  val SequenceTileTitleItem  = Css("explore-sequence-tile-title-item")

  val ButtonCopy: Css            = Css("explore-button-copy")
  val ButtonsUndo: Css           = Css("explore-buttons-undo")
  val ButtonsUndoLabel: Css      = Css("explore-buttons-undo-label")
  val ButtonSummary: Css         = Css("explore-button-summary")
  val TitleUndoButtons: Css      = Css("explore-title-undo-buttons")
  val TitleSelectColumns: Css    = Css("explore-title-select-columns")
  val TableSelectionToolbar: Css = Css("explore-table-selection-toolbar")

  val SelectColumns: Css = Css("explore-select-columns")

  val VeryCompact: Css = Css("verycompact")

  val SharedEditWarning: Css = Css("explore-shared-edit-warning")

  val StickyColumn: Css            = Css("sticky-column")
  val StickyHeader: Css            = Css("sticky-header")
  val TargetSummaryDelete: Css     = Css("explore-target-summary-delete")
  val TargetSummarySelect: Css     = Css("explore-target-summary-select")
  val TargetSummaryId: Css         = Css("explore-target-summary-id")
  val TargetSummaryType: Css       = Css("explore-target-summary-type")
  val TargetSummaryName: Css       = Css("explore-target-summary-name")
  val TargetSummarySubRowCell: Css = Css("explore-target-summary-subrow-cell")
  val ConstraintsSummaryEdit: Css  = Css("explore-constraints-summary-edit")

  val CursorPointer: Css = Css("explore-cursor-pointer")

  val ModalCloseButton: Css = Css("modal-close-button")
  val PaddedRightIcon: Css  = Css("padded-right-icon")

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
  val TargetSearchForm: Css               = Css("explore-target-search-form")
  val TargetSearchTop: Css                = Css("explore-target-search-top")
  val TargetSearchContent: Css            = Css("explore-target-search-content")
  val TargetSearchInput: Css              = Css("explore-target-search-input")
  val TargetSearchPreview: Css            = Css("explore-target-search-preview")
  val TargetSearchAladin: Css             = Css("aladin-search-target")
  val TargetSearchPreviewPlaceholder: Css = Css("explore-target-search-preview-placeholder")
  val TargetSearchResults: Css            = Css("explore-target-search-results")

  // Configuration Request Editor Popup
  val ConfigurationRequestEditorPopup: Css = Css("explore-config-request-editor")

  // Aladin Target classes
  val ScienceTarget: Css           = Css("science-target")
  val ScienceSelectedTarget: Css   = Css("science-selected-target")
  val CircleTarget: Css            = Css("circle-target")
  val BaseTarget                   = Css("base-target")
  val CrosshairTarget              = Css("crosshair-target")
  val ArrowBetweenTargets          = Css("arrow-between-targets")
  val GuideStarCandidateTarget     = Css("guide-star-candidate-target")
  val GuideStarCandidateCrowded    = Css("guide-star-candidate-target-crowded")
  val GuideStarTarget              = Css("guide-star-target")
  val OffsetPosition               = Css("offset-position")
  val ScienceOffsetPosition        = Css("science-offset-position")
  val AcquisitionOffsetPosition    = Css("acquisition-offset-position")
  val GuideSpeedFast               = Css("guide-star-fast")
  val GuideSpeedMedium             = Css("guide-star-medium")
  val GuideSpeedSlow               = Css("guide-star-slow")
  val VignettedGS                  = Css("guide-star-vignetted")
  val GuideStarCandidateTargetBase = Css("guide-star-candidate-target-base")
  val GuideStarCandidateVisible    = Css("guide-star-candidate-target-visible")
  val PMCorrectionLine             = Css("proper-motion-line")
  val PMGSCorrectionLine           = Css("proper-motion-line-gs")
  val AladinToolbox                = Css("explore-aladin-toolbox-button")
  val AladinZoomControl            = Css("explore-aladin-zoom-control")
  val AladinSearchZoomControl      = Css("explore-aladin-search-zoom-control")
  val ButtonOnAladin               = Css("explore-aladin-button")
  val AladinFullScreenButton       = Css("explore-aladin-fullscreen-button")
  val AladinSettingsMenu           = Css("explore-aladin-settings-menu")
  val BlendedSVG                   = Css("blended-svg")

  val ExplorePromptToast = Css("explore-prompt-toast")

  // For icons that will be show instead of an input during some circumstances.
  val InputReplacementIcon: Css = Css("input-replacement-icon") |+| Css("field")

  val SummaryTableToolbar: Css = Css("summary-table-toolbar")

  // TODO: to lucuma-ui
  val Header: Css      = Css("pl-header")
  val SmallHeader: Css = Header |+| lucuma.ui.primereact.LucumaPrimeStyles.Small
  // Move to PrimeReactStyles
  val Disabled: Css    = Css("p-disabled")
  val FormValue: Css   = Css("explore-value-field")

  // Timing Windows
  val TimingWindowsBody: Css                    = Css("timing-windows-body")
  val TimingWindowsEmpty: Css                   = Css("timing-windows-empty")
  val TimingWindowsList: Css                    = Css("timing-windows-list")
  val TimingWindowsTable: Css                   = Css("timing-windows-table")
  val TimingWindowEditor: Css                   = Css("timing-window-editor")
  val TimingWindowEditorHeader: Css             = Css("timing-window-editor-header")
  val TimingWindowInclusionEditor: Css          = Css("timing-window-inclusion-editor")
  val TimingWindowFromEditor: Css               = Css("timing-window-from-editor")
  val TimingWindowThroughEditor: Css            = Css("timing-window-through-editor")
  val TimingWindowEditorBody: Css               = Css("timing-window-editor-body")
  val TimingWindowEndAfter: Css                 = Css("timing-window-end-after")
  val TimingWindowRepeatEditor: Css             = Css("timing-window-repeat-editor")
  val TimingWindowRepeatEditorAlternatives: Css = Css("timing-window-repeat-editor-alternatives")
  val TimingWindowRepeatEditorNTimes: Css       = Css("timing-window-repeat-editor-n-times")
  val TimingWindowsHeader: Css                  = Css("timing-windows-header")
  val TimingWindowsInclusion: Css               = Css("timing-windows-inclusion")
  val TimingWindowInclude: Css                  = Css("timing-window-include")
  val TimingWindowExclude: Css                  = Css("timing-window-exclude")

  val SingleTileMaximized: Css = Css("explore-single-tile-maximized")

  // Program Tab
  val ProgramDetailsTile: Css     = Css("program-details-tile")
  val ProgramDetailsInfoArea: Css = Css("program-details-info-area")
  val ProgramDetailsLeft: Css     = Css("program-details-left")
  val ProgramDetailsRight: Css    = Css("program-details-right")
  val ProgramTabTable: Css        = Css("program-tab-table")
  val ProgramDetailsUsers: Css    = Css("program-details-users")

  val ProgramNotesTileBody: Css         = Css("program-notes-tile-body")
  val ProgramNotesTileTitleButtons: Css = Css("program-notes-tile-title-buttons")
  val ProgramNoteEditor: Css            = Css("program-note-editor")
  val ProgramNoteHeader: Css            = Css("program-note-header")
  val ProgramNoteTitle: Css             = Css("program-note-title")

  // Markdown Editor
  val MarkdownEditor: Css      = Css("markdown-editor")
  val MarkdownPlaceholder: Css = Css("markdown-placeholder")

  // Make a tabview take up the full height available
  val FullHeightTabView: Css = Css("full-height-tab-view")

  val FocusedInfo: Css = Css("explore-focused-info")

  val Hidden: Css    = Css("explore-hidden")
  val Stale: Css     = Css("stale")
  val ZeroValue: Css = Css("explore-zero-value")
