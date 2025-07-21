// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.TooltipOptions
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens
import org.typelevel.log4cats.Logger

object GmosImagingConfigPanel {
  // These displays are allowing the display of the short name in the chips of the
  // filter multi-select, but long names in the dropdown list. If we decide to only
  // use the long name everywhere, remove these givens and the itemTemplate in the multi-select.
  given Display[GmosNorthFilter] = Display.by(_.shortName, _.longName)
  given Display[GmosSouthFilter] = Display.by(_.shortName, _.longName)

  sealed trait GmosImagingConfigPanel[T <: ObservingMode, Input]:
    def programId: Program.Id
    def obsId: Observation.Id
    def calibrationRole: Option[CalibrationRole]
    def observingMode: Aligner[T, Input]
    def exposureTimeMode: View[Option[ExposureTimeMode]]
    def imagingRequirements: View[ScienceRequirements.Imaging]
    def revertConfig: Callback
    def sequenceChanged: Callback
    def readonly: Boolean
    def units: WavelengthUnits
    def instrument = observingMode.get.instrument

  sealed abstract class GmosImagingConfigPanelBuilder[
    T <: ObservingMode,
    Input,
    Props <: GmosImagingConfigPanel[T, Input],
    Filter: Enumerated: Display
  ] {
    protected type AA = Aligner[T, Input]

    @inline protected def isCustomized(aligner: AA): Boolean = aligner.get.isCustomized

    @inline protected def revertCustomizations(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Callback

    @inline protected def filters(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[NonEmptyList[Filter]]

    @inline protected def explicitMultipleFiltersMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[MultipleFiltersMode]]

    @inline protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosBinning]]

    @inline protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]]

    @inline protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]]

    @inline protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[List[Offset]]]

    @inline protected val filtersLens: Lens[T, NonEmptyList[Filter]]
    @inline protected val initialFiltersLens: Lens[T, NonEmptyList[Filter]]
    @inline protected val filterTypeGetter: Filter => FilterType
    @inline protected val defaultMultipleFiltersModeLens: Lens[T, MultipleFiltersMode]
    @inline protected val defaultBinningLens: Lens[T, GmosBinning]
    @inline protected val defaultReadModeGainLens: Lens[T, (GmosAmpReadMode, GmosAmpGain)]
    @inline protected val defaultRoiLens: Lens[T, GmosRoi]
    @inline protected val defaultSpatialOffsetsLens: Lens[T, List[Offset]]

    @inline protected def resolvedReadModeGainGetter: T => (GmosAmpReadMode, GmosAmpGain)

    protected given Display[(GmosAmpReadMode, GmosAmpGain)] =
      Display.by( // Shortname is in lower case for some reason
        { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
        { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
      )

    val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx              <- useContext(AppContext.ctx)
          editState        <- useStateView(ConfigEditState.View)
          localFiltersView <- useStateView(List.empty[Filter])
          _                <-
            useEffectWithDeps(filtersLens.get(props.observingMode.get).toList)(localFiltersView.set)
        yield
          import ctx.given

          val disableAdvancedEdit = editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
          val disableSimpleEdit   =
            disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit

          val defaultMultipleFiltersMode =
            defaultMultipleFiltersModeLens.get(props.observingMode.get)
          val defaultBinning             = defaultBinningLens.get(props.observingMode.get)
          val defaultReadModeGain        = defaultReadModeGainLens.get(props.observingMode.get)
          val defaultRoi                 = defaultRoiLens.get(props.observingMode.get)
          val resolvedReadModeGain       = resolvedReadModeGainGetter(props.observingMode.get)

          val filtersView                                               = filters(props.observingMode)
          val filtersGrouper: NonEmptyList[(String, Filter => Boolean)] =
            NonEmptyList.of(
              ("Broad Band", filterTypeGetter(_) === FilterType.BroadBand),
              ("Narrow Band", filterTypeGetter(_) === FilterType.NarrowBand),
              ("Combination", filterTypeGetter(_) === FilterType.Combination),
              ("Engineering", filterTypeGetter(_) === FilterType.Engineering)
            )
          val initialFilters                                            = initialFiltersLens.get(props.observingMode.get)

          <.div(
            ExploreStyles.AdvancedConfigurationGrid
          )(
            <.div(
              LucumaPrimeStyles.FormColumnCompact,
              ExploreStyles.AdvancedConfigurationCol1
            )(
              CustomizableEnumGroupedMultiSelect(
                id = "filters".refined,
                label = "Filters".some,
                helpId = Some("configuration/gmos/imaging-filters.md".refined),
                view = localFiltersView.withOnMod(l =>
                  NonEmptyList.fromList(l).fold(Callback.empty)(filtersView.set)
                ),
                defaultValue = initialFilters.toList,
                defaultFormatter = Some(Display[Filter].longName),
                groupFunctions = filtersGrouper,
                error = Option
                  .when(localFiltersView.get.isEmpty)(
                    "At least one filter is required"
                  ),
                itemTemplate = Some(si => Display[Filter].longName(si.value)),
                maxSelectedLabels = 3.some,
                selectedItemsLabel = s"${localFiltersView.get.size} selected".some,
                tooltip = localFiltersView.get.map(Display[Filter].longName).mkString("\n").some,
                tooltipOptions = TooltipOptions(showOnDisabled = true).some,
                disabled = disableSimpleEdit
              ),
              CustomizableEnumSelectOptional(
                id = "explicitMultipleFiltersMode".refined,
                view = explicitMultipleFiltersMode(props.observingMode)
                  .withDefault(defaultMultipleFiltersMode),
                defaultValue = defaultMultipleFiltersMode.some,
                label = "Multiple Filters".some,
                helpId = Some("configuration/imaging/multiple-filters-mode.md".refined),
                disabled = disableSimpleEdit
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol2)(
              ExposureTimeModeEditor(
                props.instrument.some,
                none,
                props.exposureTimeMode,
                ScienceMode.Imaging,
                props.readonly,
                props.units,
                props.calibrationRole
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol3)(
              CustomizableEnumSelectOptional(
                id = "explicitBin".refined,
                view = explicitBinning(props.observingMode).withDefault(defaultBinning),
                defaultValue = defaultBinning.some,
                label = "Binning".some,
                helpId = Some("configuration/gmos/binning.md".refined),
                disabled = disableAdvancedEdit,
                dropdownMods = ^.aria.label := "Binning"
              ),
              CustomizableEnumSelectOptional(
                id = "explicitReadMode".refined,
                view = explicitReadModeGain(props.observingMode)
                  .withDefault(defaultReadModeGain, resolvedReadModeGain),
                defaultValue = defaultReadModeGain.some,
                label = "Read Mode".some,
                helpId = Some("configuration/gmos/read-mode.md".refined),
                disabled = disableAdvancedEdit
              ),
              CustomizableEnumSelectOptional(
                id = "explicitRoi".refined,
                view = explicitRoi(props.observingMode).withDefault(defaultRoi),
                defaultValue = defaultRoi.some,
                label = "ROI".some,
                helpId = Some("configuration/gmos/roi.md".refined),
                disabled = disableAdvancedEdit
              )
            ),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = isCustomized(props.observingMode),
              revertConfig = props.revertConfig,
              revertCustomizations = revertCustomizations(props.observingMode),
              sequenceChanged = props.sequenceChanged,
              readonly = props.readonly
            )
          )
  }

  // Gmos North Imaging
  case class GmosNorthImaging(
    programId:           Program.Id,
    obsId:               Observation.Id,
    calibrationRole:     Option[CalibrationRole],
    observingMode:       Aligner[ObservingMode.GmosNorthImaging, GmosNorthImagingInput],
    exposureTimeMode:    View[Option[ExposureTimeMode]],
    imagingRequirements: View[ScienceRequirements.Imaging],
    revertConfig:        Callback,
    sequenceChanged:     Callback,
    readonly:            Boolean,
    units:               WavelengthUnits
  ) extends ReactFnProps[GmosImagingConfigPanel.GmosNorthImaging](
        GmosImagingConfigPanel.GmosNorthImaging.component
      )
      with GmosImagingConfigPanel[
        ObservingMode.GmosNorthImaging,
        GmosNorthImagingInput
      ]

  object GmosNorthImaging
      extends GmosImagingConfigPanelBuilder[
        ObservingMode.GmosNorthImaging,
        GmosNorthImagingInput,
        GmosImagingConfigPanel.GmosNorthImaging,
        GmosNorthFilter
      ] {

    @inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    @inline override protected def filters(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[NonEmptyList[GmosNorthFilter]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.filters,
        GmosNorthImagingInput.filters.modify
      )
      .view(_.toList.assign)

    @inline override protected def explicitMultipleFiltersMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[MultipleFiltersMode]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitMultipleFiltersMode,
        GmosNorthImagingInput.explicitMultipleFiltersMode.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitBin,
        GmosNorthImagingInput.explicitBin.modify
      )
      .view(_.orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosNorthImaging.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosNorthImaging.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthImagingInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    @inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosNorthImagingInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthImagingInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    @inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitRoi,
        GmosNorthImagingInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[List[Offset]]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitSpatialOffsets,
        GmosNorthImagingInput.explicitSpatialOffsets.modify
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val filtersLens               = ObservingMode.GmosNorthImaging.filters
    @inline override protected val initialFiltersLens        =
      ObservingMode.GmosNorthImaging.initialFilters
    @inline override val filterTypeGetter                    = _.filterType
    @inline protected val defaultMultipleFiltersModeLens     =
      ObservingMode.GmosNorthImaging.defaultMultipleFiltersMode
    @inline protected val defaultBinningLens                 = ObservingMode.GmosNorthImaging.defaultBin
    @inline protected val defaultReadModeGainLens            =
      (ObservingMode.GmosNorthImaging.defaultAmpReadMode,
       ObservingMode.GmosNorthImaging.defaultAmpGain
      ).disjointZip
    @inline protected val defaultRoiLens                     = ObservingMode.GmosNorthImaging.defaultRoi
    @inline override protected val defaultSpatialOffsetsLens =
      ObservingMode.GmosNorthImaging.defaultSpatialOffsets

    @inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosNorthImaging.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthImaging.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosNorthImaging.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthImaging.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }

  // Gmos South Imaging
  case class GmosSouthImaging(
    programId:           Program.Id,
    obsId:               Observation.Id,
    calibrationRole:     Option[CalibrationRole],
    observingMode:       Aligner[ObservingMode.GmosSouthImaging, GmosSouthImagingInput],
    exposureTimeMode:    View[Option[ExposureTimeMode]],
    imagingRequirements: View[ScienceRequirements.Imaging],
    revertConfig:        Callback,
    sequenceChanged:     Callback,
    readonly:            Boolean,
    units:               WavelengthUnits
  ) extends ReactFnProps[GmosImagingConfigPanel.GmosSouthImaging](
        GmosImagingConfigPanel.GmosSouthImaging.component
      )
      with GmosImagingConfigPanel[
        ObservingMode.GmosSouthImaging,
        GmosSouthImagingInput
      ]

  object GmosSouthImaging
      extends GmosImagingConfigPanelBuilder[
        ObservingMode.GmosSouthImaging,
        GmosSouthImagingInput,
        GmosImagingConfigPanel.GmosSouthImaging,
        GmosSouthFilter
      ] {

    @inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    @inline override protected def filters(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[NonEmptyList[GmosSouthFilter]] =
      aligner
        .zoom(
          ObservingMode.GmosSouthImaging.filters,
          GmosSouthImagingInput.filters.modify
        )
        .view(_.toList.assign)

    @inline override protected def explicitMultipleFiltersMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[MultipleFiltersMode]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitMultipleFiltersMode,
        GmosSouthImagingInput.explicitMultipleFiltersMode.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitBin,
        GmosSouthImagingInput.explicitBin.modify
      )
      .view(_.orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosSouthImaging.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosSouthImaging.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthImagingInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    @inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosSouthImagingInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthImagingInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    @inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitRoi,
        GmosSouthImagingInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[List[Offset]]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitSpatialOffsets,
        GmosSouthImagingInput.explicitSpatialOffsets.modify
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val filtersLens               = ObservingMode.GmosSouthImaging.filters
    @inline override protected val initialFiltersLens        =
      ObservingMode.GmosSouthImaging.initialFilters
    @inline override protected val filterTypeGetter          = _.filterType
    @inline protected val defaultMultipleFiltersModeLens     =
      ObservingMode.GmosSouthImaging.defaultMultipleFiltersMode
    @inline protected val defaultBinningLens                 = ObservingMode.GmosSouthImaging.defaultBin
    @inline protected val defaultReadModeGainLens            =
      (ObservingMode.GmosSouthImaging.defaultAmpReadMode,
       ObservingMode.GmosSouthImaging.defaultAmpGain
      ).disjointZip
    @inline protected val defaultRoiLens                     = ObservingMode.GmosSouthImaging.defaultRoi
    @inline override protected val defaultSpatialOffsetsLens =
      ObservingMode.GmosSouthImaging.defaultSpatialOffsets

    @inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosSouthImaging.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthImaging.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosSouthImaging.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthImaging.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }
}
