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
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.common.ScienceConversions.*
import explore.common.ScienceQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.ExposureTimeModeType.*
import explore.config.sequence.SequenceEditorPopup
import explore.given
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.OdbItcResult
import explore.model.ScienceMode
import explore.model.display.given
import explore.model.itc.CoverageCenterWavelength
import explore.model.reusability.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import explore.optics.*
import explore.optics.all.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormEnumDropdownOptionalView
import lucuma.ui.primereact.FormInputText
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import lucuma.utils.*
import monocle.Lens
import mouse.boolean.*
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries.*
import react.common.Css
import react.common.ReactFnProps
import react.fa.IconSize
import react.floatingui.syntax.*
import react.primereact.Button
import react.primereact.PrimeStyles
import reactST.primereact.components.{Button => CButton}
import spire.math.Bounded
import spire.math.Interval

import java.time.Duration

import scalajs.js
import scalajs.js.JSConverters.*

sealed trait AdvancedConfigurationPanel[T <: ScienceMode, Input]:
  def obsId: Observation.Id
  def title: String
  def subtitle: Option[NonEmptyString]
  def scienceMode: Aligner[T, Input]
  def spectroscopyRequirements: SpectroscopyRequirementsData
  def potITC: View[Pot[Option[OdbItcResult.Success]]]
  def editState: View[ConfigEditState]
  def confMatrix: SpectroscopyModesMatrix

sealed abstract class AdvancedConfigurationPanelBuilder[
  T <: ScienceMode,
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

  // TODO: Reverting will delete the configuration
  // @inline protected def revertCustomizations(aligner: AA)(using
  //   MonadError[IO, Throwable],
  //   Effect.Dispatch[IO],
  //   Logger[IO]
  // ): Callback

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

  protected implicit val displayBinning: Display[(XBinning, YBinning)] =
    Display.by(
      { case (x, y) => s"${x.shortName} x ${y.shortName}" },
      { case (x, y) => s"${x.longName} x ${y.longName}" }
    )

  protected implicit val displayReadModeGain: Display[(ReadMode, Gain)] =
    Display.by( // Shortname is in lower case for some reason
      { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
      { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
    )

  private val itcNoneMsg = "No ITC Results"

  private val wavelengthChangeAuditor =
    ChangeAuditor
      .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
      .allow(s => s === "0" || s === "0.")
      .decimal(3.refined)

  private case class ReadonlyData(
    coverage:   (Quantity[BigDecimal, Micrometer], Quantity[BigDecimal, Micrometer]),
    resolution: PosInt
  ) {
    val formattedCoverage: String =
      this.coverage.toList
        .map(q => "%.3f".format(q.value.setScale(3, BigDecimal.RoundingMode.DOWN)))
        .mkString(" - ")
  }

  private object ReadonlyData {
    def build(row: SpectroscopyModeRow, wavelength: Option[Wavelength]): Option[ReadonlyData] =
      wavelength.flatMap { cw =>
        if (cw >= row.minWavelength.w && cw <= row.maxWavelength.w)
          ReadonlyData(SpectroscopyModeRow.coverageInterval(cw)(row), row.resolution).some
        else
          none
      }
  }

  private def findMatrixDataFromRow(
    mode:           T,
    reqsWavelength: Option[Wavelength],
    row:            SpectroscopyModeRow
  ): Option[ReadonlyData] = (mode, row.instrument) match {
    case (m: ScienceMode.GmosNorthLongSlit, GmosNorthSpectroscopyRow(rGrating, rFpu, rFilter)) =>
      if (m.grating === rGrating && m.filter === rFilter && m.fpu === rFpu)
        ReadonlyData.build(row, reqsWavelength)
      else none
    case (m: ScienceMode.GmosSouthLongSlit, GmosSouthSpectroscopyRow(rGrating, rFpu, rFilter)) =>
      if (m.grating === rGrating && m.filter === rFilter && m.fpu === rFpu)
        ReadonlyData.build(row, reqsWavelength)
      else none
    case _                                                                                     => none
  }

  private def findMatrixData(
    mode:           T,
    reqsWavelength: Option[Wavelength],
    rows:           List[SpectroscopyModeRow]
  ): Option[ReadonlyData] =
    rows.collectFirstSome(row => findMatrixDataFromRow(mode, reqsWavelength, row))

  private def customized(original: String): VdomNode =
    <.span(
      ^.cls := "fa-layers fa-fw",
      Icons.ExclamationDiamond
        .withClass(ExploreStyles.WarningIcon)
        .withSize(IconSize.X1)
    ).withTooltip(tooltip = <.div("Customized!", <.br, s"Orginal: $original"))

  // TODO: Need a Dropdown that can be reset to a default value.
  private def customizableEnumSelect[A: Enumerated: Display](
    id:       NonEmptyString,
    view:     View[A],
    original: A,
    disabled: Boolean,
    exclude:  Set[A] = Set.empty[A]
  ) =
    val originalText = original.shortName
    <.span(
      LucumaStyles.FormField,
      PrimeStyles.InputGroup,
      FormEnumDropdownView(
        id = id,
        value = view,
        exclude = exclude,
        disabled = disabled
      ),
      (view.get =!= original).fold(customized(originalText).some, none[VdomNode])
    )

  private def customizableEnumSelectOptional[A: Enumerated: Display](
    id:       NonEmptyString,
    view:     View[Option[A]],
    original: Option[A],
    disabled: Boolean,
    exclude:  Set[A] = Set.empty[A]
  ) =
    val originalText = original.map(_.shortName).getOrElse("None")
    <.span(
      LucumaStyles.FormField,
      PrimeStyles.InputGroup,
      FormEnumDropdownOptionalView(
        id = id,
        value = view,
        exclude = exclude,
        placeholder = originalText,
        disabled = disabled
      ),
      view.get.map(_ => customized(originalText))
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
    val unitAddon   = units.map(u => u: TagMod)
    // Need the x icon, but only if not disabled
    val clearAddon  =
      if (!disabled && isCustom)
        <.span(^.cls := (LucumaStyles.BlendedAddon |+| LucumaStyles.IconTimes).htmlClass,
               ^.onClick --> value.set(originalValue)
        ).some
      else none
    val customAddon =
      if (isCustom) customized(validFormat.reverseGet(originalValue)).some
      else none
    val addons      = List(clearAddon, customAddon).flatten
    FormInputTextView(
      id = id,
      value = value,
      label = label,
      units = units.orUndefined,
      postAddons = addons,
      validFormat = validFormat,
      changeAuditor = changeAuditor,
      disabled = disabled
    )

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
    val unitAddon    = units.map(u => u: TagMod)
    val originalText = validFormat.reverseGet(originalValue.some)
    val customAddon  = value.get.map(_ => customized(originalText))
    FormInputTextView(
      id = id,
      value = value,
      label = label,
      units = units.orUndefined,
      postAddons = customAddon.toList,
      validFormat = validFormat,
      changeAuditor = changeAuditor,
      placeholder = originalText,
      disabled = disabled
    ).clearable

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // .useStateViewBy { (props, ctx) =>
      //   import ctx.given

      //   overrideExposureTimeMode(props.scienceMode).get
      //     .map(ExposureTimeModeType.fromExposureTimeMode)
      // }
      // .useEffectWithDepsBy { (props, ctx, _) =>
      //   import ctx.given

      //   overrideExposureTimeMode(props.scienceMode).get.map(
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
         props.spectroscopyRequirements.wavelengthCoverage
        )
      ) { (props, _) =>
        { case (fp, cap, fpa, res, cov) =>
          props.confMatrix.filtered(
            focalPlane = fp,
            capability = cap,
            slitWidth = fpa,
            resolution = res,
            coverage = cov.flatMap(
              _.toMicrometers.value.value.withUnit[Micrometer].toRefined[NonNegative].toOption
            )
          )
        }
      }
      // Try to find the readonly data from the spectroscopy matrix
      .useMemoBy { (props, ctx, rows) =>
        import ctx.given

        (props.spectroscopyRequirements.wavelength,
         rows,
         centralWavelength(props.scienceMode).get,
         grating(props.scienceMode).get,
         filter(props.scienceMode).get,
         fpu(props.scienceMode).get
        )
      } { (props, _, _) =>
        { case (reqsWavelength, rows, _, _, _, _) =>
          findMatrixData(props.scienceMode.get, reqsWavelength, rows)
        }
      }
      // .render { (props, ctx, exposureModeEnum, _, readonlyData) =>
      .render { (props, ctx, _, readonlyData) =>
        import ctx.given

        // val exposureModeView = overrideExposureTimeMode(props.scienceMode)

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

        val disableAdvancedEdit = props.editState.get =!= ConfigEditState.AdvancedEdit
        val disableSimpleEdit   =
          disableAdvancedEdit && props.editState.get =!= ConfigEditState.SimpleEdit

        def dithersControl(onChange: Callback): VdomElement =
          <.span("Under construction")
          // val view = explicitWavelengthDithers(props.scienceMode)
          // customizableInputTextOptional(
          //   id = "dithers".refined,
          //   value = view.withOnMod(_ => onChange),
          //   originalValue = defaultWavelengthDithersLens.get(props.scienceMode.get),
          //   label =
          //     React.Fragment("λ Dithers", HelpIcon("configuration/lambda-dithers.md".refined)),
          //   validFormat = ExploreModelValidators.dithersValidSplitEpi,
          //   changeAuditor = ChangeAuditor
          //     .bigDecimal(integers = 3.refined, decimals = 1.refined)
          //     .toSequence()
          //     .optional,
          //   units = "nm".some,
          //   disabled = disableSimpleEdit
          // )

        def offsetsControl(onChange: Callback): VdomElement = {
          val view = explicitSpatialOffsets(props.scienceMode)
          customizableInputTextOptional(
            id = "offsets".refined,
            value = view.withOnMod(_ => onChange),
            originalValue = defaultSpatialOffsetsLens.get(props.scienceMode.get),
            label = React.Fragment("Spatial Offsets",
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
          props.potITC.set(Pot.pending[Option[OdbItcResult.Success]])

        val wavelengthView           = centralWavelength(props.scienceMode)
        val initialCentralWavelength = initialCentralWavelengthLens.get(props.scienceMode.get)

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
            LucumaStyles.FormColumnCompact,
            ExploreStyles.AdvancedConfigurationCol1
          )(
            FormLabel(htmlFor = "grating".refined)(
              "Grating",
              HelpIcon("configuration/grating.md".refined)
            ),
            customizableEnumSelect(
              id = "grating".refined,
              view = grating(props.scienceMode),
              original = initialGratingLens.get(props.scienceMode.get),
              disabled = disableAdvancedEdit,
              exclude = obsoleteGratings
            ),
            FormLabel(htmlFor = "filter".refined)(
              "Filter",
              HelpIcon("configuration/filter.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "filter".refined,
              view = filter(props.scienceMode),
              original = initialFilterLens.get(props.scienceMode.get),
              disabled = disableAdvancedEdit,
              exclude = obsoleteFilters
            ),
            FormLabel(htmlFor = "fpu".refined)(
              "FPU",
              HelpIcon("configuration/fpu.md".refined)
            ),
            customizableEnumSelect(
              id = "fpu".refined,
              view = fpu(props.scienceMode),
              original = initialFpuLens.get(props.scienceMode.get),
              disabled = disableAdvancedEdit
            ),
            offsetsControl(Callback.empty)
          ),
          <.div(LucumaStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol2)(
            customizableInputText(
              id = "central-wavelength".refined,
              value = wavelengthView.withOnMod(_ => invalidateITC),
              label = React.Fragment("Wavelength",
                                     HelpIcon("configuration/central=wavelength.md".refined)
              ),
              validFormat = ExploreModelValidators.wavelengthValidWedge,
              changeAuditor = wavelengthChangeAuditor,
              units = "μm".some,
              originalValue = initialCentralWavelength,
              disabled = disableSimpleEdit
            ),
            dithersControl(Callback.empty)
            // FormLabel(htmlFor = "exposureMode".refined)(
            //   "Exposure Mode",
            //   HelpIcon("configuration/exposure-mode.md".refined)
            // ),
            // <.span(
            //   LucumaStyles.FormField,
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
          <.div(LucumaStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol3)(
            FormLabel(htmlFor = "explicitXBin".refined)(
              "Binning",
              HelpIcon("configuration/binning.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "explicitXBin".refined,
              view = explicitBinning(props.scienceMode),
              original = defaultBinningLens.get(props.scienceMode.get).some,
              disabled = disableAdvancedEdit
            ),
            FormLabel(htmlFor = "explicitReadMode".refined)(
              "Read Mode",
              HelpIcon("configuration/read-mode.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "explicitReadMode".refined,
              view = explicitReadModeGain(props.scienceMode),
              original = defaultReadModeGainLens.get(props.scienceMode.get).some,
              disabled = disableAdvancedEdit
            ),
            FormLabel(htmlFor = "explicitRoi".refined)("ROI",
                                                       HelpIcon("configuration/roi.md".refined)
            ),
            customizableEnumSelectOptional(
              id = "explicitRoi".refined,
              view = explicitRoi(props.scienceMode),
              original = defaultRoiLens.get(props.scienceMode.get).some,
              disabled = disableAdvancedEdit,
              exclude = obsoleteRois
            ),
            FormInputText(
              id = "lambda".refined,
              value = readonlyData.value.fold("Unknown")(_.resolution.toString),
              label = "λ / Δλ",
              disabled = true
            ),
            FormLabel(htmlFor = "lambdaCoverage".refined)("λ Coverage"),
            FormInputText(
              id = "lambdaCoverage".refined,
              value = readonlyData.value.fold("Unknown")(_.formattedCoverage),
              disabled = true,
              units = "nm"
            )
          ),
          <.div(ExploreStyles.AdvancedConfigurationButtons)(
            SequenceEditorPopup(
              props.obsId,
              props.title,
              props.subtitle,
              dithersControl,
              offsetsControl,
              trigger = Button(
                label = "View Sequence",
                severity = Button.Severity.Secondary
              ).compact.small
            ),
            Button(
              label = "View Suggested Configs",
              icon = Icons.ListIcon,
              severity = Button.Severity.Secondary,
              onClick = props.editState.set(ConfigEditState.TableView)
            ).compact.small
              .unless(isCustomized(props.scienceMode)),
            Button(
              label = "Revert Customizations",
              icon = Icons.TrashUnstyled,
              severity = Button.Severity.Danger,
              onClick = props.editState.set(ConfigEditState.DetailsView) >>
                // exposureModeEnum.set(none) >>
                invalidateITC // >>
              // TODO: Put back
              // revertCustomizations(props.scienceMode)
            ).compact.small
              .when(isCustomized(props.scienceMode)),
            Button(
              label = "Customize",
              icon = Icons.Edit,
              severity = Button.Severity.Secondary,
              onClick = props.editState.set(ConfigEditState.SimpleEdit)
            ).compact.small
              .when(props.editState.get === ConfigEditState.DetailsView),
            Button(
              label = "Advanced Customization",
              icon = Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
              severity = Button.Severity.Secondary,
              onClick = props.editState.set(ConfigEditState.AdvancedEdit)
            ).compact.small
              .when(props.editState.get === ConfigEditState.SimpleEdit)
          )
        )
      }
}

object AdvancedConfigurationPanel {
  sealed abstract class GmosAdvancedConfigurationPanel[
    T <: ScienceMode,
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
    obsId:                    Observation.Id,
    title:                    String,
    subtitle:                 Option[NonEmptyString],
    scienceMode:              Aligner[ScienceMode.GmosNorthLongSlit, GmosNorthLongSlitInput],
    spectroscopyRequirements: SpectroscopyRequirementsData,
    potITC:                   View[Pot[Option[OdbItcResult.Success]]],
    editState:                View[ConfigEditState],
    confMatrix:               SpectroscopyModesMatrix
  ) extends ReactFnProps[AdvancedConfigurationPanel.GmosNorthLongSlit](
        AdvancedConfigurationPanel.GmosNorthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ScienceMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput,
      ]

  object GmosNorthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput,
        AdvancedConfigurationPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu,
      ] {

    // TODO: Reverting will delete the configuration
    // @inline override protected def revertCustomizations(
    //   aligner: AA
    // )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
    //   aligner.view(_.toInput).set(ScienceMode.GmosNorthLongSlit.Empty)

    @inline override protected def centralWavelength(
      aligner: AA
    )(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength] =
      aligner
        .zoom(
          ScienceMode.GmosNorthLongSlit.centralWavelength.andThen(CoverageCenterWavelength.value),
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
          ScienceMode.GmosNorthLongSlit.grating,
          GmosNorthLongSlitInput.grating.modify
        )
        .view(_.assign)

    @inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = aligner
      .zoom(
        ScienceMode.GmosNorthLongSlit.filter,
        GmosNorthLongSlitInput.filter.modify
      )
      .view(_.orUnassign)

    @inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthFpu] = aligner
      .zoom(
        ScienceMode.GmosNorthLongSlit.fpu,
        GmosNorthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    // @inline override protected def overrideExposureTimeMode(aligner: AA)(using
    //   MonadError[IO, Throwable],
    //   Effect.Dispatch[IO],
    //   Logger[IO]
    // ): View[Option[ExposureTimeMode]] = aligner
    //   .zoom(
    //     ScienceMode.GmosNorthLongSlit.overrideExposureTimeMode,
    //     GmosNorthLongSlitInput.overrideExposureTimeMode.modify
    //   )
    //   .view(_.map(_.toInput).orUnassign)

    private val explicitXBin =
      ScienceMode.GmosNorthLongSlit.explicitXBin

    private val explicitYBin =
      ScienceMode.GmosNorthLongSlit.explicitYBin

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
      ScienceMode.GmosNorthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ScienceMode.GmosNorthLongSlit.explicitAmpGain

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
        ScienceMode.GmosNorthLongSlit.explicitRoi,
        GmosNorthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ScienceMode.GmosNorthLongSlit.explicitWavelengthDithers,
        GmosNorthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

    @inline override protected def explicitSpatialOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ScienceMode.GmosNorthLongSlit.explicitSpatialOffsets,
        GmosNorthLongSlitInput.explicitSpatialOffsets.modify _
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val initialGratingLens           = ScienceMode.GmosNorthLongSlit.initialGrating
    @inline override protected val initialFilterLens            = ScienceMode.GmosNorthLongSlit.initialFilter
    @inline override protected val initialFpuLens               = ScienceMode.GmosNorthLongSlit.initialFpu
    @inline override protected val initialCentralWavelengthLens =
      ScienceMode.GmosNorthLongSlit.initialCentralWavelength.andThen(CoverageCenterWavelength.value)
    @inline protected val defaultBinningLens                    =
      disjointZip(ScienceMode.GmosNorthLongSlit.defaultXBin,
                  ScienceMode.GmosNorthLongSlit.defaultYBin
      )
    @inline protected val defaultReadModeGainLens               =
      disjointZip(
        ScienceMode.GmosNorthLongSlit.defaultAmpReadMode,
        ScienceMode.GmosNorthLongSlit.defaultAmpGain
      )
    @inline protected val defaultRoiLens                        = ScienceMode.GmosNorthLongSlit.defaultRoi
    @inline override protected val defaultWavelengthDithersLens =
      ScienceMode.GmosNorthLongSlit.defaultWavelengthDithers
    @inline override protected val defaultSpatialOffsetsLens    =
      ScienceMode.GmosNorthLongSlit.defaultSpatialOffsets

    @inline override protected val obsoleteGratings = GmosNorthGrating.all.filter(_.obsolete).toSet
    @inline override protected val obsoleteFilters  = GmosNorthFilter.all.filter(_.obsolete).toSet
  }

// Gmos South Long Slit

  case class GmosSouthLongSlit(
    obsId:                    Observation.Id,
    title:                    String,
    subtitle:                 Option[NonEmptyString],
    scienceMode:              Aligner[ScienceMode.GmosSouthLongSlit, GmosSouthLongSlitInput],
    spectroscopyRequirements: SpectroscopyRequirementsData,
    potITC:                   View[Pot[Option[OdbItcResult.Success]]],
    editState:                View[ConfigEditState],
    confMatrix:               SpectroscopyModesMatrix
  ) extends ReactFnProps[AdvancedConfigurationPanel.GmosSouthLongSlit](
        AdvancedConfigurationPanel.GmosSouthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ScienceMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput
      ]

  object GmosSouthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput,
        AdvancedConfigurationPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu,
      ] {

    // @inline override protected def isCustomized(aligner: AA): Boolean =
    //   aligner.get =!= ScienceMode.GmosSouthLongSlit.Empty

    // @inline override protected def revertCustomizations(
    //   aligner: AA
    // )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
    //   aligner.view(_.toInput).set(ScienceMode.GmosSouthLongSlit.Empty)

    @inline override def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ScienceMode.GmosSouthLongSlit.centralWavelength.andThen(CoverageCenterWavelength.value),
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
          ScienceMode.GmosSouthLongSlit.grating,
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
          ScienceMode.GmosSouthLongSlit.filter,
          GmosSouthLongSlitInput.filter.modify
        )
        .view(_.orUnassign)

    @inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthFpu] = aligner
      .zoom(
        ScienceMode.GmosSouthLongSlit.fpu,
        GmosSouthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    // @inline override protected def overrideExposureTimeMode(aligner: AA)(using
    //   MonadError[IO, Throwable],
    //   Effect.Dispatch[IO],
    //   Logger[IO]
    // ): View[Option[ExposureTimeMode]] = aligner
    //   .zoom(
    //     ScienceMode.GmosSouthLongSlit.overrideExposureTimeMode,
    //     GmosSouthLongSlitInput.overrideExposureTimeMode.modify
    //   )
    //   .view(_.map(_.toInput).orUnassign)

    private val explicitXBin =
      ScienceMode.GmosSouthLongSlit.explicitXBin
    private val explicitYBin =
      ScienceMode.GmosSouthLongSlit.explicitYBin

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
      ScienceMode.GmosSouthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ScienceMode.GmosSouthLongSlit.explicitAmpGain

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
        ScienceMode.GmosSouthLongSlit.explicitRoi,
        GmosSouthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(ScienceMode.GmosSouthLongSlit.explicitWavelengthDithers,
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
        ScienceMode.GmosSouthLongSlit.explicitSpatialOffsets,
        GmosSouthLongSlitInput.explicitSpatialOffsets.modify _
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val initialGratingLens           = ScienceMode.GmosSouthLongSlit.initialGrating
    @inline override protected val initialFilterLens            = ScienceMode.GmosSouthLongSlit.initialFilter
    @inline override protected val initialFpuLens               = ScienceMode.GmosSouthLongSlit.initialFpu
    @inline override protected val initialCentralWavelengthLens =
      ScienceMode.GmosSouthLongSlit.initialCentralWavelength.andThen(CoverageCenterWavelength.value)
    @inline protected val defaultBinningLens                    =
      disjointZip(ScienceMode.GmosSouthLongSlit.defaultXBin,
                  ScienceMode.GmosSouthLongSlit.defaultYBin
      )
    @inline protected val defaultReadModeGainLens               =
      disjointZip(
        ScienceMode.GmosSouthLongSlit.defaultAmpReadMode,
        ScienceMode.GmosSouthLongSlit.defaultAmpGain
      )
    @inline protected val defaultRoiLens                        = ScienceMode.GmosSouthLongSlit.defaultRoi
    @inline override protected val defaultWavelengthDithersLens =
      ScienceMode.GmosSouthLongSlit.defaultWavelengthDithers
    @inline override protected val defaultSpatialOffsetsLens    =
      ScienceMode.GmosSouthLongSlit.defaultSpatialOffsets

    @inline override protected val obsoleteGratings = GmosSouthGrating.all.filter(_.obsolete).toSet
    @inline override protected val obsoleteFilters  = GmosSouthFilter.all.filter(_.obsolete).toSet
  }
}
