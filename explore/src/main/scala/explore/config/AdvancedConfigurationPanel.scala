// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import coulomb.Quantity
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.ModeCommonWavelengths
import explore.modes.ModeSlitSize
import explore.modes.ModeWavelength
import explore.modes.SlitLength
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Program
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.IconSize
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.PrimeStyles
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.ObservationDB.Types.WavelengthInput
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens
import mouse.boolean.*
import org.typelevel.log4cats.Logger

import scalajs.js
import scalajs.js.JSConverters.*

sealed trait AdvancedConfigurationPanel[T <: ObservingMode, Input]:
  def programId: Program.Id
  def obsId: Observation.Id
  def observingMode: Aligner[T, Input]
  def spectroscopyRequirements: ScienceRequirements.Spectroscopy
  def deleteConfig: Callback
  def confMatrix: SpectroscopyModesMatrix
  def selectedConfig: View[Option[BasicConfigAndItc]]
  def sequenceChanged: Callback
  def readonly: Boolean

sealed abstract class AdvancedConfigurationPanelBuilder[
  T <: ObservingMode,
  Input,
  Props <: AdvancedConfigurationPanel[T, Input],
  Grating: Enumerated: Display,
  Filter: Enumerated: Display,
  Fpu: Enumerated: Display,
  XBinning: Enumerated: Display,
  YBinning: Enumerated: Display,
  ReadMode: Enumerated: Display,
  Gain: Enumerated: Display,
  Roi: Enumerated: Display
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

  // @inline protected def overrideExposureTimeMode(aligner: AA)(using
  //   MonadError[IO, Throwable],
  //   Effect.Dispatch[IO],
  //   Logger[IO]
  // ): View[Option[ExposureTimeMode]]

  @inline protected def explicitBinning(aligner: AA)(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[(XBinning, YBinning)]]

  @inline protected def explicitReadModeGain(aligner: AA)(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[(ReadMode, Gain)]]

  @inline protected def explicitRoi(aligner: AA)(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[Roi]]

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
  @inline protected val defaultBinningLens: Lens[T, (XBinning, YBinning)]
  @inline protected val defaultReadModeGainLens: Lens[T, (ReadMode, Gain)]
  @inline protected val defaultRoiLens: Lens[T, Roi]
  @inline protected val defaultWavelengthDithersLens: Lens[T, NonEmptyList[WavelengthDither]]
  @inline protected val defaultSpatialOffsetsLens: Lens[T, NonEmptyList[Offset.Q]]

  @inline protected val obsoleteGratings: Set[Grating]
  @inline protected val obsoleteFilters: Set[Filter]
  @inline protected val obsoleteRois: Set[Roi]

  protected given Display[(XBinning, YBinning)] =
    Display.by(
      { case (x, y) => s"${x.shortName} x ${y.shortName}" },
      { case (x, y) => s"${x.longName} x ${y.longName}" }
    )

  protected given Display[(ReadMode, Gain)] =
    Display.by( // Shortname is in lower case for some reason
      { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
      { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
    )

  private val wavelengthChangeAuditor =
    ChangeAuditor
      .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
      .allow(s => s === "0" || s === "0.")
      .decimal(3.refined)

  private case class ModeData private (
    centralWavelength: Wavelength,
    interval:          BoundedInterval[Wavelength],
    resolution:        PosInt,
    λmin:              ModeWavelength,
    λmax:              ModeWavelength,
    λdelta:            WavelengthDelta
  ) extends ModeCommonWavelengths

  private object ModeData {
    def build(row: SpectroscopyModeRow, wavelength: Option[Wavelength]): Option[ModeData] =
      wavelength.flatMap { cw =>
        if (cw >= row.λmin.value && cw <= row.λmax.value)
          ModeCommonWavelengths
            .wavelengthInterval(cw)(row)
            .map(interval =>
              ModeData(
                cw,
                interval,
                row.resolution,
                row.λmin,
                row.λmax,
                row.λdelta
              )
            )
        else
          none
      }
  }

  private def findMatrixDataFromRow(
    mode:           T,
    reqsWavelength: Option[Wavelength],
    row:            SpectroscopyModeRow
  ): Option[ModeData] =
    reqsWavelength.flatMap(cw =>
      (mode, row.instrument) match
        case (m: ObservingMode.GmosNorthLongSlit,
              GmosNorthSpectroscopyRow(rGrating, rFpu, rFilter, _)
            ) if m.grating === rGrating && m.filter === rFilter && m.fpu === rFpu =>
          ModeData.build(row, reqsWavelength)
        case (m: ObservingMode.GmosSouthLongSlit,
              GmosSouthSpectroscopyRow(rGrating, rFpu, rFilter, _)
            ) if m.grating === rGrating && m.filter === rFilter && m.fpu === rFpu =>
          ModeData.build(row, reqsWavelength)
        case _ => none
    )

  private def findMatrixData(
    mode:           T,
    reqsWavelength: Option[Wavelength],
    rows:           List[SpectroscopyModeRow]
  ): Option[ModeData] =
    rows.collectFirstSome(row => findMatrixDataFromRow(mode, reqsWavelength, row))

  // If the view contains `none`, `get` returns the default value. When setting,
  // if the new value is the default value, set it to none.
  extension [A: Eq](view: View[Option[A]])
    private def withDefault(default: A): View[Option[A]] =
      view.zoom(_.orElse(default.some))(f =>
        b => f(b).flatMap(newB => if (newB === default) none else newB.some)
      )

  private def customized(original: String, toRevert: Callback): VdomNode =
    <.span(
      ^.cls := "fa-layers fa-fw",
      Icons.ExclamationDiamond
        .withClass(ExploreStyles.WarningIcon)
        .withSize(IconSize.X1),
      ^.onClick --> toRevert
    ).withTooltip(tooltip =
      <.div("Customized!", <.br, s"Orginal: $original", <.br, "Click to revert.")
    )

  private def customizableEnumSelect[A: Enumerated: Display](
    id:       NonEmptyString,
    view:     View[A],
    original: A,
    disabled: Boolean,
    exclude:  Set[A] = Set.empty[A]
  ) =
    val originalText = original.shortName
    <.span(
      LucumaPrimeStyles.FormField,
      PrimeStyles.InputGroup,
      FormEnumDropdownView(
        id = id,
        value = view,
        exclude = exclude,
        disabled = disabled
      ),
      (view.get =!= original).fold(
        <.span(PrimeStyles.InputGroupAddon, customized(originalText, view.set(original)).some),
        none[VdomNode]
      )
    )

  private def customizableEnumSelectOptional[A: Enumerated: Display](
    id:              NonEmptyString,
    view:            View[Option[A]],
    original:        Option[A],
    disabled:        Boolean,
    exclude:         Set[A] = Set.empty[A],
    showClear:       Boolean = false,
    resetToOriginal: Boolean = false // resets to `none` on false
  ) =
    val originalText = original.map(_.shortName).getOrElse("None")
    <.span(
      LucumaPrimeStyles.FormField,
      PrimeStyles.InputGroup,
      FormEnumDropdownOptionalView(
        id = id,
        value = view,
        exclude = exclude,
        disabled = disabled,
        showClear = showClear
      ),
      <.span(PrimeStyles.InputGroupAddon,
             customized(originalText, view.set(if (resetToOriginal) original else none))
      )
        .when(view.get =!= original)
    )

  private def customizableInputText[A: Eq](
    id:            NonEmptyString,
    value:         View[A],
    validFormat:   InputValidFormat[A],
    changeAuditor: ChangeAuditor,
    label:         TagMod,
    originalValue: A,
    units:         Option[String] = None,
    disabled:      Boolean = false
  ) =
    val isCustom    = value.get =!= originalValue
    val customAddon =
      if (isCustom) customized(validFormat.reverseGet(originalValue), value.set(originalValue)).some
      else none
    FormInputTextView(
      id = id,
      value = value,
      label = label,
      units = units.orUndefined,
      postAddons = customAddon.toList,
      validFormat = validFormat,
      changeAuditor = changeAuditor,
      disabled = disabled
    ).withMods(^.autoComplete.off)

  private def customizableInputTextOptional[A: Eq](
    id:            NonEmptyString,
    value:         View[Option[A]],
    validFormat:   InputValidFormat[Option[A]],
    changeAuditor: ChangeAuditor,
    label:         TagMod,
    originalValue: A,
    units:         Option[String] = None,
    disabled:      Boolean = false
  ) =
    val originalText = validFormat.reverseGet(originalValue.some)
    val customAddon  = value.get.map(_ => customized(originalText, value.set(none)))
    FormInputTextView(
      id = id,
      value = value.withDefault(originalValue),
      label = label,
      units = units.orUndefined,
      postAddons = customAddon.toList,
      validFormat = validFormat,
      changeAuditor = changeAuditor,
      disabled = disabled
    ).clearable(^.autoComplete.off)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // .useStateViewBy { (props, ctx) =>
      //   import ctx.given

      //   overrideExposureTimeMode(props.observingMode).get
      //     .map(ExposureTimeModeType.fromExposureTimeMode)
      // }
      // .useEffectWithDepsBy { (props, ctx, _) =>
      //   import ctx.given

      //   overrideExposureTimeMode(props.observingMode).get.map(
      //     ExposureTimeModeType.fromExposureTimeMode
      //   )
      // }((_, _, exposureModeEnum) =>
      //   newExpModeEnum =>
      //     if (exposureModeEnum.get =!= newExpModeEnum) exposureModeEnum.set(newExpModeEnum)
      //     else Callback.empty
      // )
      // filter the spectroscopy matrix by the requirements that don't get overridden
      // by the advanced config (wavelength, for example).
      .useMemoBy((props, _) =>
        (props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.capability,
         props.spectroscopyRequirements.focalPlaneAngle,
         props.spectroscopyRequirements.resolution,
         props.spectroscopyRequirements.wavelengthCoverage,
         props.confMatrix.matrix.length
        )
      ) { (props, _) =>
        { case (fp, cap, fpa, res, rng, _) =>
          props.confMatrix.filtered(
            focalPlane = fp,
            capability = cap,
            slitLength = fpa.map(s => SlitLength(ModeSlitSize(s))),
            resolution = res,
            range = rng
          )
        }
      }
      // Try to find the mode row from the spectroscopy matrix
      .useMemoBy { (props, ctx, rows) =>
        import ctx.given

        (props.spectroscopyRequirements.wavelength,
         rows,
         centralWavelength(props.observingMode).get,
         grating(props.observingMode).get,
         filter(props.observingMode).get,
         fpu(props.observingMode).get
        )
      } { (props, _, _) =>
        { case (reqsWavelength, rows, _, _, _, _) =>
          findMatrixData(props.observingMode.get, reqsWavelength, rows)
        }
      }
      .useStateView(ConfigEditState.View)
      .render { (props, ctx, _, modeData, editState) =>
        import ctx.given

        // val exposureModeView = overrideExposureTimeMode(props.observingMode)

        // val exposureCountView: Option[View[NonNegInt]] =
        //   exposureModeView
        //     .mapValue((v: View[ExposureTimeMode]) => v.zoom(ExposureTimeMode.exposureCount).asView)
        //     .flatten

        // val exposureTimeView: Option[View[NonNegDuration]] =
        //   exposureModeView
        //     .mapValue((v: View[ExposureTimeMode]) => v.zoom(ExposureTimeMode.exposureTime).asView)
        //     .flatten

        // val signalToNoiseView: Option[View[PosBigDecimal]] =
        //   exposureModeView
        //     .mapValue((v: View[ExposureTimeMode]) =>
        //       v.zoom(ExposureTimeMode.signalToNoiseValue).asView
        //     )
        //     .flatten

        val disableAdvancedEdit = editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
        val disableSimpleEdit   =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit

        val centralWavelengthView    = centralWavelength(props.observingMode)
        val initialCentralWavelength = initialCentralWavelengthLens.get(props.observingMode.get)

        val adjustedInterval =
          modeData.value.flatMap(
            ModeCommonWavelengths.wavelengthInterval(centralWavelengthView.get)
          )

        val defaultBinning      = defaultBinningLens.get(props.observingMode.get)
        val defaultReadModeGain = defaultReadModeGainLens.get(props.observingMode.get)
        val defaultRoi          = defaultRoiLens.get(props.observingMode.get)

        val validDithers = modeData.value
          .map(mode =>
            ExploreModelValidators.dithersValidWedge(centralWavelengthView.get,
                                                     mode.λmin.value,
                                                     mode.λmax.value,
                                                     mode.λdelta
            )
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
          customizableInputTextOptional(
            id = "dithers".refined,
            value = view.withOnMod(_ => onChange),
            originalValue = default,
            label =
              React.Fragment("λ Dithers", HelpIcon("configuration/lambda-dithers.md".refined)),
            validFormat = validDithers,
            changeAuditor = ChangeAuditor
              .bigDecimal(integers = 3.refined, decimals = 1.refined)
              .toSequence()
              .optional,
            units = "nm".some,
            disabled = disableSimpleEdit
          )

        def offsetsControl(onChange: Callback): VdomElement = {
          val default = defaultSpatialOffsetsLens.get(props.observingMode.get)
          val view    = explicitSpatialOffsets(props.observingMode)
          customizableInputTextOptional(
            id = "offsets".refined,
            value = view.withOnMod(_ => onChange),
            originalValue = default,
            label = React.Fragment(
              "Spatial Offsets",
              HelpIcon("configuration/spatial-offsets.md".refined)
            ),
            validFormat = ExploreModelValidators.offsetQNELValidWedge,
            changeAuditor = ChangeAuditor
              .bigDecimal(integers = 3.refined, decimals = 2.refined)
              .toSequence()
              .optional,
            units = "arcsec".some,
            disabled = disableSimpleEdit
          )
        }

        val invalidateITC: Callback =
          Callback.empty

        // val originalSignalToNoiseText =
        //   props.spectroscopyRequirements.signalToNoise.fold("None")(sn =>
        //     s"S/N ${InputValidWedge.truncatedPosBigDecimal(0.refined).reverseGet(sn)}"
        //   )

        // def onModeMod(modType: Option[ExposureTimeModeType]): Callback = {
        //   val optITC: Option[OdbItcResult.Success] = props.potITC.get.toOption.flatten
        //   val oetm                                 = modType.map {
        //     case ExposureTimeModeType.SignalToNoise =>
        //       val sn: PosBigDecimal = signalToNoiseView
        //         .map(_.get)
        //         .orElse(props.spectroscopyRequirements.signalToNoise)
        //         .orElse(optITC.map(_.signalToNoise))
        //         .getOrElse(BigDecimal(100).refined)
        //       ExposureTimeMode.SignalToNoise(sn)
        //     case ExposureTimeModeType.FixedExposure =>
        //       val time  = exposureTimeView
        //         .map(_.get)
        //         .orElse(optITC.map(_.exposureTime))
        //         .getOrElse(zeroDuration)
        //       val count = exposureCountView
        //         .map(_.get)
        //         .orElse(optITC.map(_.exposures))
        //         .getOrElse(NonNegInt.unsafeFrom(0))
        //       ExposureTimeMode.FixedExposure(count, time)
        //   }
        //   exposureModeView.set(oetm) >> invalidateITC
        // }

        <.div(
          ExploreStyles.AdvancedConfigurationGrid
        )(
          <.div(
            LucumaPrimeStyles.FormColumnCompact,
            ExploreStyles.AdvancedConfigurationCol1
          )(
            FormLabel(htmlFor = "grating".refined)(
              "Grating",
              HelpIcon("configuration/grating.md".refined)
            ),
            customizableEnumSelect(
              id = "grating".refined,
              view = grating(props.observingMode),
              original = initialGratingLens.get(props.observingMode.get),
              disabled = disableAdvancedEdit,
              exclude = obsoleteGratings
            ),
            FormLabel(htmlFor = "filter".refined)(
              "Filter",
              HelpIcon("configuration/filter.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "filter".refined,
              view = filter(props.observingMode),
              original = initialFilterLens.get(props.observingMode.get),
              disabled = disableAdvancedEdit,
              exclude = obsoleteFilters,
              showClear = true,
              resetToOriginal = true
            ),
            FormLabel(htmlFor = "fpu".refined)(
              "FPU",
              HelpIcon("configuration/fpu.md".refined)
            ),
            customizableEnumSelect(
              id = "fpu".refined,
              view = fpu(props.observingMode),
              original = initialFpuLens.get(props.observingMode.get),
              disabled = disableAdvancedEdit
            ),
            offsetsControl(props.sequenceChanged)
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol2)(
            customizableInputText(
              id = "central-wavelength".refined,
              value = centralWavelengthView.withOnMod(_ => invalidateITC),
              label = React.Fragment("Central Wavelength",
                                     HelpIcon("configuration/central=wavelength.md".refined)
              ),
              validFormat = ExploreModelValidators.wavelengthValidWedge,
              changeAuditor = wavelengthChangeAuditor,
              units = "μm".some,
              originalValue = initialCentralWavelength,
              disabled = disableSimpleEdit
            ),
            dithersControl(props.sequenceChanged)
            // FormLabel(htmlFor = "exposureMode".refined)(
            //   "Exposure Mode",
            //   HelpIcon("configuration/exposure-mode.md".refined)
            // ),
            // <.span(
            //   LucumaPrimeStyles.FormField,
            //   PrimeStyles.InputGroup,
            //   FormEnumDropdownOptionalView(
            //     id = "exposureMode".refined,
            //     value = exposureModeEnum.withOnMod(onModeMod _),
            //     disabled = disableSimpleEdit,
            //     placeholder = originalSignalToNoiseText
            //   ),
            //   exposureModeEnum.get.map(_ => customized(originalSignalToNoiseText))
            // ),
            // FormLabel(htmlFor = "signalToNoise".refined)(
            //   "S/N",
            //   HelpIcon("configuration/signal-to-noise.md".refined),
            //   ExploreStyles.IndentLabel
            // ),
            // signalToNoiseView
            //   .map(v =>
            //     FormInputTextView(
            //       id = "signalToNoise".refined,
            //       value = v.withOnMod(_ => invalidateITC),
            //       validFormat = InputValidWedge.truncatedPosBigDecimal(0.refined),
            //       changeAuditor = ChangeAuditor.posInt,
            //       disabled = disableSimpleEdit
            //     ): VdomNode
            //   )
            //   .getOrElse(
            //     potRender[Option[PosBigDecimal]](
            //       valueRender = osn => {
            //         val value = osn.fold(itcNoneMsg)(sn =>
            //           InputValidWedge.truncatedPosBigDecimal(0.refined).reverseGet(sn)
            //         )
            //         FormInputText(id = "signalToNoise".refined, value = value, disabled = true)
            //       },
            //       pendingRender = <.div(ExploreStyles.InputReplacementIcon,
            //                             Icons.Spinner.withSpin(true)
            //       ): VdomNode
            //     )(props.potITC.get.map(_.map(_.signalToNoise)))
            //   ),
            // FormLabel(htmlFor = "exposureTime".refined)(
            //   "Exp Time",
            //   HelpIcon("configuration/exposure-time.md".refined),
            //   ExploreStyles.IndentLabel
            // ),
            // exposureTimeView
            //   .map(v =>
            //     FormInputTextView(
            //       id = "exposureTime".refined,
            //       value = v
            //         .zoomSplitEpi[NonNegInt](nonNegDurationSecondsSplitEpi)
            //         .withOnMod(_ => invalidateITC),
            //       validFormat = InputValidSplitEpi.refinedInt[NonNegative],
            //       changeAuditor = ChangeAuditor.refinedInt[NonNegative](),
            //       units = "sec",
            //       disabled = disableSimpleEdit
            //     ): TagMod
            //   )
            //   .getOrElse(
            //     potRender[Option[NonNegDuration]](
            //       valueRender = ot => {
            //         val value =
            //           ot.fold(itcNoneMsg)(t => nonNegDurationSecondsSplitEpi.get(t).toString)
            //         FormInputText(id = "exposureTime".refined,
            //                       value = value,
            //                       disabled = true,
            //                       units = "sec"
            //         )
            //       },
            //       pendingRender = <.div(
            //         ExploreStyles.InputReplacementIcon,
            //         Icons.Spinner.withSpin(true)
            //       ): VdomNode
            //     )(props.potITC.get.map(_.map(_.exposureTime)))
            //   ),
            // FormLabel(htmlFor = "exposureCount".refined)(
            //   "Exp Count",
            //   HelpIcon("configuration/exposure-count.md".refined),
            //   ExploreStyles.IndentLabel
            // ),
            // exposureCountView
            //   .map(v =>
            //     FormInputTextView(
            //       id = "exposureCount".refined,
            //       value = v.withOnMod(_ => invalidateITC),
            //       validFormat = InputValidSplitEpi.refinedInt[NonNegative],
            //       changeAuditor = ChangeAuditor.refinedInt[NonNegative](),
            //       disabled = disableSimpleEdit
            //     ): TagMod
            //   )
            //   .getOrElse(
            //     potRender[Option[NonNegInt]](
            //       valueRender = oe => {
            //         val value = oe.fold(itcNoneMsg)(_.toString)
            //         FormInputText(id = "exposureCount".refined, value = value, disabled = true)
            //       },
            //       pendingRender = <.div(
            //         ExploreStyles.InputReplacementIcon,
            //         Icons.Spinner.withSpin(true)
            //       ): VdomNode
            //     )(props.potITC.get.map(_.map(_.exposures)))
            //   )
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol3)(
            FormLabel(htmlFor = "explicitXBin".refined)(
              "Binning",
              HelpIcon("configuration/binning.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "explicitXBin".refined,
              view = explicitBinning(props.observingMode).withDefault(defaultBinning),
              original = defaultBinning.some,
              disabled = disableAdvancedEdit
            ),
            FormLabel(htmlFor = "explicitReadMode".refined)(
              "Read Mode",
              HelpIcon("configuration/read-mode.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "explicitReadMode".refined,
              view = explicitReadModeGain(props.observingMode).withDefault(defaultReadModeGain),
              original = defaultReadModeGain.some,
              disabled = disableAdvancedEdit
            ),
            FormLabel(htmlFor = "explicitRoi".refined)("ROI",
                                                       HelpIcon("configuration/roi.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "explicitRoi".refined,
              view = explicitRoi(props.observingMode).withDefault(defaultRoi),
              original = defaultRoi.some,
              disabled = disableAdvancedEdit,
              exclude = obsoleteRois
            ),
            FormLabel(htmlFor = "lambda".refined)("λ / Δλ"),
            <.label(^.id := "lambda",
                    ExploreStyles.FormValue,
                    s"${modeData.value.fold("Unknown")(_.resolution.toString)}"
            ),
            FormLabel(htmlFor = "lambdaInterval".refined)("λ Interval"),
            <.label(^.id := "lambdaInterval",
                    ExploreStyles.FormValue,
                    s"${adjustedInterval.fold("Unknown")(_.shortName)} nm"
            )
          ),
          <.div(ExploreStyles.AdvancedConfigurationButtons)(
            Button(
              label = "Revert Configuration",
              icon = Icons.ListIcon,
              severity = Button.Severity.Secondary,
              onClick = props.selectedConfig.mod(c =>
                BasicConfigAndItc(
                  props.observingMode.get.toBasicConfiguration,
                  c.flatMap(_.itcResult.flatMap(_.toOption.map(_.asRight)))
                ).some
              )
                >> props.deleteConfig
            ).compact.small
              .unless(isCustomized(props.observingMode)),
            Button(
              label = "Revert Customizations",
              icon = Icons.TrashUnstyled,
              severity = Button.Severity.Danger,
              onClick = props.sequenceChanged *> editState.set(ConfigEditState.View) >>
                // exposureModeEnum.set(none) >>
                invalidateITC >>
                revertCustomizations(props.observingMode)
            ).compact.small
              .when(isCustomized(props.observingMode)),
            Button(
              label = "Customize",
              icon = Icons.Edit,
              severity = Button.Severity.Secondary,
              onClick = editState.set(ConfigEditState.SimpleEdit)
            ).compact.small
              .when(editState.get === ConfigEditState.View),
            Button(
              label = "Advanced Customization",
              icon = Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
              severity = Button.Severity.Secondary,
              onClick = editState.set(ConfigEditState.AdvancedEdit)
            ).compact.small
              .when(editState.get === ConfigEditState.SimpleEdit)
          ).unless(props.readonly)
        )
      }
}

object AdvancedConfigurationPanel {
  sealed abstract class GmosAdvancedConfigurationPanel[
    T <: ObservingMode,
    Input,
    Props <: AdvancedConfigurationPanel[T, Input],
    Grating: Enumerated: Display,
    Filter: Enumerated: Display,
    Fpu: Enumerated: Display
  ] extends AdvancedConfigurationPanelBuilder[
        T,
        Input,
        Props,
        Grating,
        Filter,
        Fpu,
        GmosXBinning,
        GmosYBinning,
        GmosAmpReadMode,
        GmosAmpGain,
        GmosRoi
      ] {
    @inline override protected val obsoleteRois = GmosRoi.all.filter(_.obsolete).toSet
  }

  // Gmos North Long Slit
  case class GmosNorthLongSlit(
    programId:                Program.Id,
    obsId:                    Observation.Id,
    observingMode:            Aligner[ObservingMode.GmosNorthLongSlit, GmosNorthLongSlitInput],
    spectroscopyRequirements: ScienceRequirements.Spectroscopy,
    deleteConfig:             Callback,
    confMatrix:               SpectroscopyModesMatrix,
    selectedConfig:           View[Option[BasicConfigAndItc]],
    sequenceChanged:          Callback,
    readonly:                 Boolean
  ) extends ReactFnProps[AdvancedConfigurationPanel.GmosNorthLongSlit](
        AdvancedConfigurationPanel.GmosNorthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput
      ]

  object GmosNorthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput,
        AdvancedConfigurationPanel.GmosNorthLongSlit,
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
          ObservingMode.GmosNorthLongSlit.centralWavelength.andThen(CentralWavelength.value),
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

    // @inline override protected def overrideExposureTimeMode(aligner: AA)(using
    //   MonadError[IO, Throwable],
    //   Effect.Dispatch[IO],
    //   Logger[IO]
    // ): View[Option[ExposureTimeMode]] = aligner
    //   .zoom(
    //     ObservingMode.GmosNorthLongSlit.overrideExposureTimeMode,
    //     GmosNorthLongSlitInput.overrideExposureTimeMode.modify
    //   )
    //   .view(_.map(_.toInput).orUnassign)

    private val explicitXBin =
      ObservingMode.GmosNorthLongSlit.explicitXBin

    private val explicitYBin =
      ObservingMode.GmosNorthLongSlit.explicitYBin

    private def binningAligner(
      aligner: AA
    ): Aligner[Option[(GmosXBinning, GmosYBinning)], GmosNorthLongSlitInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitXBin, explicitYBin), f => i => f(i))

    @inline override protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosXBinning, GmosYBinning)]] =
      binningAligner(aligner)
        .viewMod { oxy =>
          val xy = oxy.unzip
          GmosNorthLongSlitInput.explicitXBin
            .replace(xy._1.orUnassign)
            .andThen(GmosNorthLongSlitInput.explicitYBin.replace(xy._2.orUnassign))
        }

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
      ObservingMode.GmosNorthLongSlit.initialCentralWavelength.andThen(CentralWavelength.value)
    @inline protected val defaultBinningLens                    =
      (ObservingMode.GmosNorthLongSlit.defaultXBin,
       ObservingMode.GmosNorthLongSlit.defaultYBin
      ).disjointZip
    @inline protected val defaultReadModeGainLens               =
      (ObservingMode.GmosNorthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosNorthLongSlit.defaultAmpGain
      ).disjointZip
    @inline protected val defaultRoiLens                        = ObservingMode.GmosNorthLongSlit.defaultRoi
    @inline override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosNorthLongSlit.defaultWavelengthDithers
    @inline override protected val defaultSpatialOffsetsLens    =
      ObservingMode.GmosNorthLongSlit.defaultSpatialOffsets

    @inline override protected val obsoleteGratings = GmosNorthGrating.all.filter(_.obsolete).toSet
    @inline override protected val obsoleteFilters  = GmosNorthFilter.all.filter(_.obsolete).toSet
  }

// Gmos South Long Slit

  case class GmosSouthLongSlit(
    programId:                Program.Id,
    obsId:                    Observation.Id,
    observingMode:            Aligner[ObservingMode.GmosSouthLongSlit, GmosSouthLongSlitInput],
    spectroscopyRequirements: ScienceRequirements.Spectroscopy,
    deleteConfig:             Callback,
    confMatrix:               SpectroscopyModesMatrix,
    selectedConfig:           View[Option[BasicConfigAndItc]],
    sequenceChanged:          Callback,
    readonly:                 Boolean
  ) extends ReactFnProps[AdvancedConfigurationPanel.GmosSouthLongSlit](
        AdvancedConfigurationPanel.GmosSouthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput
      ]

  object GmosSouthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput,
        AdvancedConfigurationPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu
      ] {

    // @inline override protected def isCustomized(aligner: AA): Boolean =
    //   aligner.get =!= ObservingMode.GmosSouthLongSlit.Empty

    @inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    @inline override def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.centralWavelength.andThen(CentralWavelength.value),
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

    // @inline override protected def overrideExposureTimeMode(aligner: AA)(using
    //   MonadError[IO, Throwable],
    //   Effect.Dispatch[IO],
    //   Logger[IO]
    // ): View[Option[ExposureTimeMode]] = aligner
    //   .zoom(
    //     ObservingMode.GmosSouthLongSlit.overrideExposureTimeMode,
    //     GmosSouthLongSlitInput.overrideExposureTimeMode.modify
    //   )
    //   .view(_.map(_.toInput).orUnassign)

    private val explicitXBin =
      ObservingMode.GmosSouthLongSlit.explicitXBin
    private val explicitYBin =
      ObservingMode.GmosSouthLongSlit.explicitYBin

    private def binningAligner(
      aligner: AA
    ): Aligner[Option[(GmosXBinning, GmosYBinning)], GmosSouthLongSlitInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitXBin, explicitYBin), f => i => f(i))

    @inline override protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosXBinning, GmosYBinning)]] =
      binningAligner(aligner)
        .viewMod { oxy =>
          val xy = oxy.unzip
          GmosSouthLongSlitInput.explicitXBin
            .replace(xy._1.orUnassign)
            .andThen(GmosSouthLongSlitInput.explicitYBin.replace(xy._2.orUnassign))
        }

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
      ObservingMode.GmosSouthLongSlit.initialCentralWavelength.andThen(CentralWavelength.value)
    @inline protected val defaultBinningLens                    =
      (ObservingMode.GmosSouthLongSlit.defaultXBin,
       ObservingMode.GmosSouthLongSlit.defaultYBin
      ).disjointZip
    @inline protected val defaultReadModeGainLens               =
      (ObservingMode.GmosSouthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosSouthLongSlit.defaultAmpGain
      ).disjointZip
    @inline protected val defaultRoiLens                        = ObservingMode.GmosSouthLongSlit.defaultRoi
    @inline override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosSouthLongSlit.defaultWavelengthDithers
    @inline override protected val defaultSpatialOffsetsLens    =
      ObservingMode.GmosSouthLongSlit.defaultSpatialOffsets

    @inline override protected val obsoleteGratings = GmosSouthGrating.all.filter(_.obsolete).toSet
    @inline override protected val obsoleteFilters  = GmosSouthFilter.all.filter(_.obsolete).toSet
  }
}
