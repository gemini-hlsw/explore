// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import coulomb.Quantity
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.ModeWavelength
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens
import org.typelevel.log4cats.Logger

object GmosLongslitConfigPanel {
  sealed trait GmosLongslitConfigPanel[T <: ObservingMode, Input]:
    def programId: Program.Id
    def obsId: Observation.Id
    def calibrationRole: Option[CalibrationRole]
    def observingMode: Aligner[T, Input]
    def exposureTimeMode: View[Option[ExposureTimeMode]]
    def spectroscopyRequirements: View[ScienceRequirements.Spectroscopy]
    def revertConfig: Callback
    def confMatrix: SpectroscopyModesMatrix
    def sequenceChanged: Callback
    def readonly: Boolean
    def units: WavelengthUnits
    def instrument = observingMode.get.instrument

  sealed abstract class GmosLongslitConfigPanelBuilder[
    T <: ObservingMode,
    Input,
    Props <: GmosLongslitConfigPanel[T, Input],
    Grating: Enumerated: Display,
    Filter: Enumerated: Display,
    Fpu: Enumerated: Display
  ] {
    protected type AA = Aligner[T, Input]

    @inline protected def isCustomized(aligner: AA): Boolean = aligner.get.isCustomized

    @inline protected def revertCustomizations(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Callback

    @inline protected def centralWavelength(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength]

    @inline protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Grating]

    @inline protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Filter]]

    @inline protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Fpu]

    @inline protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]]

    @inline protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]]

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

    @inline protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[
      Option[NonEmptyList[WavelengthDither]]
    ]

    @inline protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]]

    @inline protected val initialGratingLens: Lens[T, Grating]
    @inline protected val initialFilterLens: Lens[T, Option[Filter]]
    @inline protected val initialFpuLens: Lens[T, Fpu]
    @inline protected val initialCentralWavelengthLens: Lens[T, Wavelength]
    @inline protected val defaultXBinningLens: Lens[T, GmosXBinning]
    @inline protected val defaultYBinningLens: Lens[T, GmosYBinning]
    @inline protected val defaultReadModeGainLens: Lens[T, (GmosAmpReadMode, GmosAmpGain)]
    @inline protected val defaultRoiLens: Lens[T, GmosRoi]
    @inline protected val defaultWavelengthDithersLens: Lens[T, NonEmptyList[WavelengthDither]]
    @inline protected val defaultSpatialOffsetsLens: Lens[T, NonEmptyList[Offset.Q]]

    @inline protected def resolvedReadModeGainGetter: T => (GmosAmpReadMode, GmosAmpGain)

    protected given Display[(GmosAmpReadMode, GmosAmpGain)] =
      Display.by( // Shortname is in lower case for some reason
        { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
        { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
      )

    val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx       <- useContext(AppContext.ctx)
          modeData  <- useModeData(
                         props.confMatrix,
                         props.spectroscopyRequirements.get,
                         props.observingMode.get
                       )
          editState <- useStateView(ConfigEditState.View)
        yield
          import ctx.given

          val disableAdvancedEdit = editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
          val disableSimpleEdit   =
            disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit

          val centralWavelengthView    = centralWavelength(props.observingMode)
          val initialCentralWavelength = initialCentralWavelengthLens.get(props.observingMode.get)

          val defaultXBinning      = defaultXBinningLens.get(props.observingMode.get)
          val defaultYBinning      = defaultYBinningLens.get(props.observingMode.get)
          val defaultReadModeGain  = defaultReadModeGainLens.get(props.observingMode.get)
          val defaultRoi           = defaultRoiLens.get(props.observingMode.get)
          val resolvedReadModeGain = resolvedReadModeGainGetter(props.observingMode.get)

          val validDithers = modeData
            .map(mode =>
              ExploreModelValidators
                .dithersValidWedge(centralWavelengthView.get, mode.λmin.value, mode.λmax.value)
            )
            .getOrElse(
              ExploreModelValidators.ditherValidWedge
            )
            .toNel(",".refined)
            .withErrorMessage(_ => "Invalid wavelength dither values".refined)
            .optional

          def dithersControl(onChange: Callback): VdomElement =
            val default = defaultWavelengthDithersLens.get(props.observingMode.get)
            val view    = explicitWavelengthDithers(props.observingMode)
            CustomizableInputTextOptional(
              id = "dithers".refined,
              value = view.withOnMod(_ => onChange),
              defaultValue = default,
              label = React.Fragment("λ Dithers",
                                     HelpIcon("configuration/gmos/lambda-dithers.md".refined)
              ),
              validFormat = validDithers,
              changeAuditor = ChangeAuditor
                .bigDecimal(integers = 3.refined, decimals = 1.refined)
                .toSequence()
                .optional,
              units = "nm".some,
              disabled = disableSimpleEdit
            )

          <.div(
            ExploreStyles.AdvancedConfigurationGrid
          )(
            <.div(
              LucumaPrimeStyles.FormColumnCompact,
              ExploreStyles.AdvancedConfigurationCol1
            )(
              CustomizableEnumSelect(
                id = "grating".refined,
                view = grating(props.observingMode),
                defaultValue = initialGratingLens.get(props.observingMode.get),
                label = "Grating".some,
                helpId = Some("configuration/gmos/grating.md".refined),
                disabled = disableAdvancedEdit
              ),
              CustomizableEnumSelectOptional(
                id = "filter".refined,
                view = filter(props.observingMode),
                defaultValue = initialFilterLens.get(props.observingMode.get),
                label = "Filter".some,
                helpId = Some("configuration/gmos/filter.md".refined),
                disabled = disableAdvancedEdit,
                showClear = true,
                resetToOriginal = true
              ),
              CustomizableEnumSelect(
                id = "fpu".refined,
                view = fpu(props.observingMode),
                defaultValue = initialFpuLens.get(props.observingMode.get),
                label = "FPU".some,
                helpId = Some("configuration/gmos/fpu.md".refined),
                disabled = disableAdvancedEdit
              ),
              OffsetsControl(explicitSpatialOffsets(props.observingMode),
                             defaultSpatialOffsetsLens.get(props.observingMode.get),
                             props.sequenceChanged,
                             disableSimpleEdit
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol2)(
              CustomizableInputText(
                id = "central-wavelength".refined,
                value = centralWavelengthView,
                label = React.Fragment("Central Wavelength",
                                       HelpIcon("configuration/gmos/central=wavelength.md".refined)
                ),
                units = props.units.symbol.some,
                validFormat = props.units.toInputFormat,
                changeAuditor = props.units.toAuditor,
                defaultValue = initialCentralWavelength,
                disabled = disableSimpleEdit
              ),
              dithersControl(props.sequenceChanged),
              ExposureTimeModeEditor(
                props.instrument.some,
                props.spectroscopyRequirements.get.wavelength,
                props.exposureTimeMode,
                ScienceMode.Spectroscopy,
                props.readonly,
                props.units,
                props.calibrationRole
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol3)(
              // Provide better accessibility by using aria-label directly
              // on the dropdowns so X and Y binning are correctly labeled.
              <.label(
                LucumaPrimeStyles.FormFieldLabel,
                "Binning",
                HelpIcon("configuration/gmos/binning.md".refined)
              ),
              <.div(
                ExploreStyles.AdvancedConfigurationBinning,
                CustomizableEnumSelectOptional(
                  id = "explicitXBin".refined,
                  view = explicitXBinning(props.observingMode).withDefault(defaultXBinning),
                  defaultValue = defaultXBinning.some,
                  disabled = disableAdvancedEdit,
                  dropdownMods = ^.aria.label := "X Binning"
                ),
                <.label("x"),
                CustomizableEnumSelectOptional(
                  id = "explicitYBin".refined,
                  view = explicitYBinning(props.observingMode).withDefault(defaultYBinning),
                  defaultValue = defaultYBinning.some,
                  disabled = disableAdvancedEdit,
                  dropdownMods = ^.aria.label := "Y Binning"
                )
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
              ),
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = centralWavelengthView.get,
                units = props.units
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

  // Gmos North Long Slit
  case class GmosNorthLongSlit(
    programId:                Program.Id,
    obsId:                    Observation.Id,
    calibrationRole:          Option[CalibrationRole],
    observingMode:            Aligner[ObservingMode.GmosNorthLongSlit, GmosNorthLongSlitInput],
    exposureTimeMode:         View[Option[ExposureTimeMode]],
    spectroscopyRequirements: View[ScienceRequirements.Spectroscopy],
    revertConfig:             Callback,
    confMatrix:               SpectroscopyModesMatrix,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits
  ) extends ReactFnProps[GmosLongslitConfigPanel.GmosNorthLongSlit](
        GmosLongslitConfigPanel.GmosNorthLongSlit.component
      )
      with GmosLongslitConfigPanel[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput
      ]

  object GmosNorthLongSlit
      extends GmosLongslitConfigPanelBuilder[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput,
        GmosLongslitConfigPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu
      ] {

    @inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    @inline override protected def centralWavelength(
      aligner: AA
    )(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
          GmosNorthLongSlitInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    @inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.grating,
          GmosNorthLongSlitInput.grating.modify
        )
        .view(_.assign)

    @inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.filter,
        GmosNorthLongSlitInput.filter.modify
      )
      .view(_.orUnassign)

    @inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthFpu] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.fpu,
        GmosNorthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    @inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitXBin,
        GmosNorthLongSlitInput.explicitXBin.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitYBin,
        GmosNorthLongSlitInput.explicitYBin.modify
      )
      .view(_.orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosNorthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosNorthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthLongSlitInput] =
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
          GmosNorthLongSlitInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    @inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitRoi,
        GmosNorthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitWavelengthDithers,
        GmosNorthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

    @inline override protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitSpatialOffsets,
        GmosNorthLongSlitInput.explicitSpatialOffsets.modify
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val initialGratingLens           =
      ObservingMode.GmosNorthLongSlit.initialGrating
    @inline override protected val initialFilterLens            = ObservingMode.GmosNorthLongSlit.initialFilter
    @inline override protected val initialFpuLens               = ObservingMode.GmosNorthLongSlit.initialFpu
    @inline override protected val initialCentralWavelengthLens =
      ObservingMode.GmosNorthLongSlit.initialCentralWavelength.andThen(CentralWavelength.Value)
    @inline protected val defaultBinningLens                    =
      (ObservingMode.GmosNorthLongSlit.defaultXBin,
       ObservingMode.GmosNorthLongSlit.defaultYBin
      ).disjointZip
    @inline protected val defaultReadModeGainLens               =
      (ObservingMode.GmosNorthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosNorthLongSlit.defaultAmpGain
      ).disjointZip
    @inline protected val defaultXBinningLens                   = ObservingMode.GmosNorthLongSlit.defaultXBin
    @inline protected val defaultYBinningLens                   = ObservingMode.GmosNorthLongSlit.defaultYBin
    @inline protected val defaultRoiLens                        = ObservingMode.GmosNorthLongSlit.defaultRoi
    @inline override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosNorthLongSlit.defaultWavelengthDithers
    @inline override protected val defaultSpatialOffsetsLens    =
      ObservingMode.GmosNorthLongSlit.defaultSpatialOffsets

    @inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosNorthLongSlit.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosNorthLongSlit.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }

// Gmos South Long Slit

  case class GmosSouthLongSlit(
    programId:                Program.Id,
    obsId:                    Observation.Id,
    calibrationRole:          Option[CalibrationRole],
    observingMode:            Aligner[ObservingMode.GmosSouthLongSlit, GmosSouthLongSlitInput],
    exposureTimeMode:         View[Option[ExposureTimeMode]],
    spectroscopyRequirements: View[ScienceRequirements.Spectroscopy],
    revertConfig:             Callback,
    confMatrix:               SpectroscopyModesMatrix,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits
  ) extends ReactFnProps[GmosLongslitConfigPanel.GmosSouthLongSlit](
        GmosLongslitConfigPanel.GmosSouthLongSlit.component
      )
      with GmosLongslitConfigPanel[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput
      ]

  object GmosSouthLongSlit
      extends GmosLongslitConfigPanelBuilder[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput,
        GmosLongslitConfigPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu
      ] {

    @inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    @inline override def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
          GmosSouthLongSlitInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    @inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.grating,
          GmosSouthLongSlitInput.grating.modify
        )
        .view(_.assign)

    @inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.filter,
          GmosSouthLongSlitInput.filter.modify
        )
        .view(_.orUnassign)

    @inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthFpu] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.fpu,
        GmosSouthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    @inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitXBin,
        GmosSouthLongSlitInput.explicitXBin.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitYBin,
        GmosSouthLongSlitInput.explicitYBin.modify
      )
      .view(_.orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosSouthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosSouthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthLongSlitInput] =
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
          GmosSouthLongSlitInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    @inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitRoi,
        GmosSouthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitWavelengthDithers,
        GmosSouthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(
        _.map(
          _.map(d => WavelengthDitherInput(picometers = d.toPicometers.value.assign)).toList
        ).orUnassign
      )

    @inline override protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitSpatialOffsets,
        GmosSouthLongSlitInput.explicitSpatialOffsets.modify
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val initialGratingLens           =
      ObservingMode.GmosSouthLongSlit.initialGrating
    @inline override protected val initialFilterLens            = ObservingMode.GmosSouthLongSlit.initialFilter
    @inline override protected val initialFpuLens               = ObservingMode.GmosSouthLongSlit.initialFpu
    @inline override protected val initialCentralWavelengthLens =
      ObservingMode.GmosSouthLongSlit.initialCentralWavelength.andThen(CentralWavelength.Value)
    @inline protected val defaultBinningLens                    =
      (ObservingMode.GmosSouthLongSlit.defaultXBin,
       ObservingMode.GmosSouthLongSlit.defaultYBin
      ).disjointZip
    @inline protected val defaultXBinningLens                   = ObservingMode.GmosSouthLongSlit.defaultXBin
    @inline protected val defaultYBinningLens                   = ObservingMode.GmosSouthLongSlit.defaultYBin
    @inline protected val defaultReadModeGainLens               =
      (ObservingMode.GmosSouthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosSouthLongSlit.defaultAmpGain
      ).disjointZip
    @inline protected val defaultRoiLens                        = ObservingMode.GmosSouthLongSlit.defaultRoi
    @inline override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosSouthLongSlit.defaultWavelengthDithers
    @inline override protected val defaultSpatialOffsetsLens    =
      ObservingMode.GmosSouthLongSlit.defaultSpatialOffsets

    @inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosSouthLongSlit.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosSouthLongSlit.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }
}
