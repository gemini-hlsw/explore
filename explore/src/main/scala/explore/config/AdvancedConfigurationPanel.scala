// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all.*
import clue.data.syntax.*
import coulomb.Quantity
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
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
import explore.common.ObsQueries.*
import explore.common.ScienceQueries.*
import explore.components.HelpIcon
import explore.components.InputWithUnits
import lucuma.ui.primereact.EnumOptionalDropdownView
import lucuma.ui.primereact.FormInputText
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import explore.components.ui.ExploreStyles
import explore.config.ExposureTimeModeType.*
import explore.implicits.*
import explore.model.DitherNanoMeters
import explore.model.ExploreModelValidators
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.display.given
import explore.model.reusability.*
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import explore.optics.*
import explore.optics.all.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.NonNegDuration
import lucuma.core.model.Observation
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.utils.*
import monocle.Lens
import mouse.boolean.*
import queries.schemas.implicits.*
import react.common.Css
import react.common.ReactFnProps
import react.fa.IconSize
import react.floatingui.syntax.*
import react.primereact.Button
import react.primereact.Padding
import react.primereact.PrimeStyles
import spire.math.Bounded
import spire.math.Interval

import java.time.Duration

import scalajs.js
import scalajs.js.JSConverters.*

sealed trait AdvancedConfigurationPanel[T <: ScienceModeAdvanced, S <: ScienceModeBasic, Input] {
  val obsId: Observation.Id
  val title: String
  val subtitle: Option[NonEmptyString]
  val scienceModeAdvanced: Aligner[T, Input]
  val scienceModeBasic: S
  val spectroscopyRequirements: SpectroscopyRequirementsData
  val potITC: View[Pot[Option[ITCSuccess]]]
  val editState: View[ConfigEditState]
  val confMatrix: SpectroscopyModesMatrix

  implicit val ctx: AppContextIO
}

sealed abstract class AdvancedConfigurationPanelBuilder[
  T <: ScienceModeAdvanced,
  S <: ScienceModeBasic: Reusability,
  Input,
  Props <: AdvancedConfigurationPanel[T, S, Input],
  Grating: Enumerated: Display,
  Filter: Enumerated: Display,
  Fpu: Enumerated: Display,
  XBinning: Enumerated: Display,
  YBinning: Enumerated: Display,
  ReadMode: Enumerated: Display,
  Gain: Enumerated: Display,
  Roi: Enumerated: Display
] {
  type AA = Aligner[T, Input]

  @inline protected def isCustomized(aligner:         AA)(implicit ctx: AppContextIO): Boolean
  @inline protected def revertCustomizations(aligner: AA)(implicit ctx: AppContextIO): Callback

  @inline protected def overrideWavelength(aligner: AA)(implicit
    ctx:                                            AppContextIO
  ): View[Option[Wavelength]]

  @inline protected def overrideGrating(aligner: AA)(implicit
    ctx:                                         AppContextIO
  ): View[Option[Grating]]

  @inline protected def overrideFilter(aligner: AA)(implicit
    ctx:                                        AppContextIO
  ): View[Option[Filter]]

  @inline protected def overrideFpu(aligner: AA)(implicit ctx: AppContextIO): View[Option[Fpu]]

  @inline protected def overrideExposureTimeMode(aligner: AA)(implicit
    ctx:                                                  AppContextIO
  ): View[Option[ExposureTimeMode]]

  @inline protected def explicitBinning(aligner: AA)(implicit
    ctx:                                         AppContextIO
  ): View[Option[(XBinning, YBinning)]]

  @inline protected def explicitReadModeGain(aligner: AA)(implicit
    ctx:                                              AppContextIO
  ): View[Option[(ReadMode, Gain)]]

  @inline protected def explicitRoi(aligner: AA)(implicit ctx: AppContextIO): View[Option[Roi]]

  @inline protected def explicitWavelengthDithers(aligner: AA)(implicit ctx: AppContextIO): View[
    Option[NonEmptyList[DitherNanoMeters]]
  ]

  @inline protected def explicitSpatialOffsets(aligner: AA)(implicit
    ctx:                                                AppContextIO
  ): View[Option[NonEmptyList[Offset.Q]]]

  @inline protected val gratingLens: Lens[S, Grating]
  @inline protected val filterLens: Lens[S, Option[Filter]]
  @inline protected val fpuLens: Lens[S, Fpu]

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

  val itcNoneMsg = "No ITC Results"

  val wavelengthChangeAuditor =
    ChangeAuditor
      .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
      .allow(s => s === "0" || s === "0.")
      .decimal(3.refined)

  private case class ReadonlyData(
    coverage:   Interval[Quantity[BigDecimal, Micrometer]],
    resolution: PosInt
  ) {
    val formattedCoverage: String = this.coverage match {
      case Bounded(a, b, _) =>
        List(a, b)
          .map(q => "%.3f".format(q.value.setScale(3, BigDecimal.RoundingMode.DOWN)))
          .mkString(" - ")
      case _                =>
        "-"
    }
  }

  private object ReadonlyData {
    def build(row: SpectroscopyModeRow, wavelength: Option[Wavelength]): Option[ReadonlyData] =
      if (wavelength.forall(w => w >= row.minWavelength.w && w <= row.maxWavelength.w))
        ReadonlyData(SpectroscopyModeRow.coverageInterval(wavelength)(row), row.resolution).some
      else
        none
  }

  private def findMatrixDataFromRow(
    basic:          S,
    advanced:       T,
    reqsWavelength: Option[Wavelength],
    row:            SpectroscopyModeRow
  ): Option[ReadonlyData] = (basic, advanced, row.instrument) match {
    case (ScienceModeBasic.GmosNorthLongSlit(bGrating, bFilter, bFpu),
          ScienceModeAdvanced.GmosNorthLongSlit(aWavelength,
                                                aGrating,
                                                aFilter,
                                                aFpu,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _
          ),
          GmosNorthSpectroscopyRow(rGrating, rFpu, rFilter)
        ) =>
      val wavelength = aWavelength.orElse(reqsWavelength)
      val grating    = aGrating.getOrElse(bGrating)
      val filter     = aFilter.orElse(bFilter)
      val fpu        = aFpu.getOrElse(bFpu)
      if (grating === rGrating && filter === rFilter && fpu === rFpu)
        ReadonlyData.build(row, wavelength)
      else none
    case (ScienceModeBasic.GmosSouthLongSlit(bGrating, bFilter, bFpu),
          ScienceModeAdvanced.GmosSouthLongSlit(aWavelength,
                                                aGrating,
                                                aFilter,
                                                aFpu,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _
          ),
          GmosSouthSpectroscopyRow(rGrating, rFpu, rFilter)
        ) =>
      val wavelength = aWavelength.orElse(reqsWavelength)
      val grating    = aGrating.getOrElse(bGrating)
      val filter     = aFilter.orElse(bFilter)
      val fpu        = aFpu.getOrElse(bFpu)
      if (grating === rGrating && filter === rFilter && fpu === rFpu)
        ReadonlyData.build(row, wavelength)
      else none
    case _ => None
  }

  private def findMatrixData(
    basic:          S,
    advanced:       T,
    reqsWavelength: Option[Wavelength],
    rows:           List[SpectroscopyModeRow]
  ): Option[ReadonlyData] =
    rows.collectFirstSome(row => findMatrixDataFromRow(basic, advanced, reqsWavelength, row))

  private def customized(original: String): VdomNode =
    <.span(
      ^.cls := "fa-layers fa-fw",
      Icons.ExclamationDiamond
        .clazz(ExploreStyles.WarningIcon)
        .size(IconSize.X1)
    ).withTooltip(tooltip = <.div("Customized!", <.br, s"Orginal: $original"))

  private def customizableEnumSelect[A: Enumerated: Display](
    id:             NonEmptyString,
    view:           View[Option[A]],
    original:       Option[A],
    disabled:       Boolean,
    exclude:        Set[A] = Set.empty[A],
    unknownDefault: Boolean =
      false // TODO: Remove and simplify when we get all of the default values from the server
  ) = {
    val originalText = original.map(_.shortName).getOrElse(unknownDefault.fold("Unknown", "None"))
    <.span(
      LucumaStyles.FormField,
      PrimeStyles.InputGroup,
      EnumOptionalDropdownView(
        id = id,
        value = view,
        exclude = exclude,
        placeholder = unknownDefault.fold(js.undefined, originalText),
        disabled = disabled
      ),
      view.get.map(_ => customized(originalText))
    )
  }

  private def customizableInputText[A: Eq](
    id:            NonEmptyString,
    value:         View[Option[A]],
    validFormat:   InputValidFormat[Option[A]],
    changeAuditor: ChangeAuditor,
    label:         TagMod,
    originalText:  Option[String] = None,
    units:         Option[String] = None,
    disabled:      Boolean = false
  ) = {
    val unitAddon   = units.map(u => u: TagMod)
    val customAddon = value.get.map(_ => customized(originalText.getOrElse("Unknown")))
    FormInputTextView(
      id = id,
      value = value,
      label = label,
      postAddons = List(unitAddon, customAddon).flatten,
      validFormat = validFormat,
      changeAuditor = changeAuditor,
      placeholder = originalText.orUndefined,
      disabled = disabled
    )
  }

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy { props =>
        import props.given
        overrideExposureTimeMode(props.scienceModeAdvanced).get
          .map(ExposureTimeModeType.fromExposureTimeMode)
      }
      .useEffectWithDepsBy { (props, _) =>
        import props.given
        overrideExposureTimeMode(props.scienceModeAdvanced).get.map(
          ExposureTimeModeType.fromExposureTimeMode
        )
      }((_, exposureModeEnum) =>
        newExpModeEnum =>
          if (exposureModeEnum.get =!= newExpModeEnum) exposureModeEnum.set(newExpModeEnum)
          else Callback.empty
      )
      // filter the spectroscopy matrix by the requirements that don't get overridden
      // by the advanced config (wavelength, for example).
      .useMemoBy((props, _) =>
        (props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.capabilities,
         props.spectroscopyRequirements.focalPlaneAngle,
         props.spectroscopyRequirements.resolution,
         props.spectroscopyRequirements.wavelengthCoverage
        )
      ) { (props, _) =>
        { case (fp, cap, fpa, res, cov) =>
          props.confMatrix.filtered(
            focalPlane = fp,
            capabilities = cap,
            slitWidth = fpa,
            resolution = res,
            coverage = cov.flatMap(_.micrometer.toValue[BigDecimal].toRefined[NonNegative].toOption)
          )
        }
      }
      // Try to find the readonly data from the spectroscopy matrix
      .useMemoBy { (props, _, rows) =>
        import props.given
        val advanced = props.scienceModeAdvanced
        (props.scienceModeBasic,
         props.spectroscopyRequirements.wavelength,
         rows,
         overrideWavelength(advanced).get,
         overrideGrating(advanced).get,
         overrideFilter(advanced).get,
         overrideFpu(advanced).get
        )
      } { (props, _, _) =>
        { case (basic, reqsWavelength, rows, _, _, _, _) =>
          findMatrixData(basic, props.scienceModeAdvanced.get, reqsWavelength, rows)
        }
      }
      .render {
        (
          props,
          exposureModeEnum,
          _,
          readonlyData
        ) =>
          import props.given

          val exposureModeView = overrideExposureTimeMode(props.scienceModeAdvanced)

          val exposureCountView: Option[View[NonNegInt]] =
            exposureModeView
              .mapValue((v: View[ExposureTimeMode]) =>
                v.zoom(ExposureTimeMode.exposureCount).asView
              )
              .flatten

          val exposureTimeView: Option[View[NonNegDuration]] =
            exposureModeView
              .mapValue((v: View[ExposureTimeMode]) => v.zoom(ExposureTimeMode.exposureTime).asView)
              .flatten

          val signalToNoiseView: Option[View[PosBigDecimal]] =
            exposureModeView
              .mapValue((v: View[ExposureTimeMode]) =>
                v.zoom(ExposureTimeMode.signalToNoiseValue).asView
              )
              .flatten

          val disableAdvancedEdit = props.editState.get =!= ConfigEditState.AdvancedEdit
          val disableSimpleEdit   =
            disableAdvancedEdit && props.editState.get =!= ConfigEditState.SimpleEdit

          def dithersControl(onChange: Callback): VdomElement = {
            val view = explicitWavelengthDithers(props.scienceModeAdvanced)
            customizableInputText(
              id = "dithers".refined,
              value = view.withOnMod(_ => onChange),
              label =
                React.Fragment("λ Dithers", HelpIcon("configuration/lambda-dithers.md".refined)),
              validFormat = ExploreModelValidators.dithersValidSplitEpi,
              changeAuditor = ChangeAuditor
                .bigDecimal(integers = 3.refined, decimals = 1.refined)
                .toSequence()
                .optional,
              units = "nm".some,
              disabled = disableSimpleEdit
            )
          }

          def offsetsControl(onChange: Callback): VdomElement = {
            val view = explicitSpatialOffsets(props.scienceModeAdvanced)
            customizableInputText(
              id = "offsets".refined,
              value = view.withOnMod(_ => onChange),
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
            props.potITC.set(Pot.pending[Option[ITCSuccess]])

          val zeroDuration: NonNegDuration = NonNegDuration.unsafeFrom(Duration.ofMillis(0))

          val wavelengthView         = overrideWavelength(props.scienceModeAdvanced)
          val originalWavelengthText =
            props.spectroscopyRequirements.wavelength.fold("None")(w =>
              f"${Wavelength.decimalMicrometers.reverseGet(w)}%.3f"
            )

          val originalSignalToNoiseText =
            props.spectroscopyRequirements.signalToNoise.fold("None")(sn =>
              s"S/N ${InputValidWedge.truncatedPosBigDecimal(0.refined).reverseGet(sn)}"
            )

          def onModeMod(modType: Option[ExposureTimeModeType]): Callback = {
            val optITC: Option[ITCSuccess] = props.potITC.get.toOption.flatten
            val oetm                       = modType.map {
              case ExposureTimeModeType.SignalToNoise =>
                val sn: PosBigDecimal = signalToNoiseView
                  .map(_.get)
                  .orElse(props.spectroscopyRequirements.signalToNoise)
                  .orElse(optITC.map(_.signalToNoise))
                  .getOrElse(BigDecimal(100).refined)
                ExposureTimeMode.SignalToNoise(sn)
              case ExposureTimeModeType.FixedExposure =>
                val time  = exposureTimeView
                  .map(_.get)
                  .orElse(optITC.map(_.exposureTime))
                  .getOrElse(zeroDuration)
                val count = exposureCountView
                  .map(_.get)
                  .orElse(optITC.map(_.exposures))
                  .getOrElse(NonNegInt.unsafeFrom(0))
                ExposureTimeMode.FixedExposure(count, time)
            }
            exposureModeView.set(oetm) >> invalidateITC
          }

          <.div(
            ExploreStyles.AdvancedConfigurationGrid
          )(
            <.div(
              LucumaStyles.FormColumnCompact,
              ExploreStyles.AdvancedConfigurationCol1
            )(
              FormLabel(htmlFor = "override-grating".refined)(
                "Grating",
                HelpIcon("configuration/grating.md".refined)
              ),
              customizableEnumSelect(
                id = "override-grating".refined,
                view = overrideGrating(props.scienceModeAdvanced),
                original = gratingLens.get(props.scienceModeBasic).some,
                disabled = disableAdvancedEdit,
                exclude = obsoleteGratings
              ),
              FormLabel(htmlFor = "override-filter".refined)(
                "Filter",
                HelpIcon("configuration/filter.md".refined)
              ),
              customizableEnumSelect(
                id = "override-filter".refined,
                view = overrideFilter(props.scienceModeAdvanced),
                original = filterLens.get(props.scienceModeBasic),
                disabled = disableAdvancedEdit,
                exclude = obsoleteFilters
              ),
              FormLabel(htmlFor = "override-fpu".refined)(
                "FPU",
                HelpIcon("configuration/fpu.md".refined)
              ),
              customizableEnumSelect(
                id = "override-fpu".refined,
                view = overrideFpu(props.scienceModeAdvanced),
                original = fpuLens.get(props.scienceModeBasic).some,
                disabled = disableAdvancedEdit
              ),
              offsetsControl(Callback.empty)
            ),
            <.div(LucumaStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol2)(
              customizableInputText(
                id = "override-wavelength".refined,
                value = wavelengthView.withOnMod(_ => invalidateITC),
                label =
                  React.Fragment("Wavelength", HelpIcon("configuration/wavelength.md".refined)),
                validFormat = ExploreModelValidators.wavelengthValidWedge.optional,
                changeAuditor = wavelengthChangeAuditor.optional,
                units = "μm".some,
                originalText = originalWavelengthText.some,
                disabled = disableSimpleEdit
              ),
              dithersControl(Callback.empty),
              FormLabel(htmlFor = "exposureMode".refined)(
                "Exposure Mode",
                HelpIcon("configuration/exposure-mode.md".refined)
              ),
              <.span(
                LucumaStyles.FormField,
                PrimeStyles.InputGroup,
                EnumOptionalDropdownView(
                  id = "exposureMode".refined,
                  value = exposureModeEnum.withOnMod(onModeMod _),
                  disabled = disableSimpleEdit,
                  placeholder = originalSignalToNoiseText
                ),
                exposureModeEnum.get.map(_ => customized(originalSignalToNoiseText))
              ),
              FormLabel(htmlFor = "signalToNoise".refined)(
                "S/N",
                HelpIcon("configuration/signal-to-noise.md".refined),
                ExploreStyles.IndentLabel
              ),
              signalToNoiseView
                .map(v =>
                  FormInputTextView(
                    id = "signalToNoise".refined,
                    value = v.withOnMod(_ => invalidateITC),
                    validFormat = InputValidWedge.truncatedPosBigDecimal(0.refined),
                    changeAuditor = ChangeAuditor.posInt,
                    disabled = disableSimpleEdit
                  ): VdomNode
                )
                .getOrElse(
                  potRender[Option[PosBigDecimal]](
                    valueRender = osn => {
                      val value = osn.fold(itcNoneMsg)(sn =>
                        InputValidWedge.truncatedPosBigDecimal(0.refined).reverseGet(sn)
                      )
                      FormInputText(id = "signalToNoise".refined, value = value, disabled = true)
                    },
                    pendingRender =
                      <.div(ExploreStyles.InputReplacementIcon, Icons.Spinner.spin(true)): VdomNode
                  )(props.potITC.get.map(_.map(_.signalToNoise)))
                ),
              FormLabel(htmlFor = "exposureTime".refined)(
                "Exp Time",
                HelpIcon("configuration/exposure-time.md".refined),
                ExploreStyles.IndentLabel
              ),
              exposureTimeView
                .map(v =>
                  FormInputTextView(
                    id = "exposureTime".refined,
                    value = v
                      .zoomSplitEpi[NonNegInt](nonNegDurationSecondsSplitEpi)
                      .withOnMod(_ => invalidateITC),
                    validFormat = InputValidSplitEpi.refinedInt[NonNegative],
                    changeAuditor = ChangeAuditor.refinedInt[NonNegative](),
                    postAddons = List("sec"),
                    disabled = disableSimpleEdit
                  ): TagMod
                )
                .getOrElse(
                  potRender[Option[NonNegDuration]](
                    valueRender = ot => {
                      val value =
                        ot.fold(itcNoneMsg)(t => nonNegDurationSecondsSplitEpi.get(t).toString)
                      FormInputText(id = "exposureTime".refined,
                                    value = value,
                                    disabled = true,
                                    postAddons = List("sec")
                      )
                    },
                    pendingRender =
                      <.div(ExploreStyles.InputReplacementIcon, Icons.Spinner.spin(true)): VdomNode
                  )(props.potITC.get.map(_.map(_.exposureTime)))
                ),
              FormLabel(htmlFor = "exposureCount".refined)(
                "Exp Count",
                HelpIcon("configuration/exposure-count.md".refined),
                ExploreStyles.IndentLabel
              ),
              exposureCountView
                .map(v =>
                  FormInputTextView(
                    id = "exposureCount".refined,
                    value = v.withOnMod(_ => invalidateITC),
                    validFormat = InputValidSplitEpi.refinedInt[NonNegative],
                    changeAuditor = ChangeAuditor.refinedInt[NonNegative](),
                    disabled = disableSimpleEdit
                  ): TagMod
                )
                .getOrElse(
                  potRender[Option[NonNegInt]](
                    valueRender = oe => {
                      val value = oe.fold(itcNoneMsg)(_.toString)
                      FormInputText(id = "exposureCount".refined, value = value, disabled = true)
                    },
                    pendingRender =
                      <.div(ExploreStyles.InputReplacementIcon, Icons.Spinner.spin(true)): VdomNode
                  )(props.potITC.get.map(_.map(_.exposures)))
                )
            ),
            <.div(LucumaStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol3)(
              FormLabel(htmlFor = "explicitXBin".refined)(
                "Binning",
                HelpIcon("configuration/binning.md".refined)
              ),
              customizableEnumSelect(
                id = "explicitXBin".refined,
                view = explicitBinning(props.scienceModeAdvanced),
                original = none,
                disabled = disableAdvancedEdit,
                unknownDefault = true
              ),
              FormLabel(htmlFor = "explicitReadMode".refined)(
                "Read Mode",
                HelpIcon("configuration/read-mode.md".refined)
              ),
              customizableEnumSelect(
                id = "explicitReadMode".refined,
                view = explicitReadModeGain(props.scienceModeAdvanced),
                original = none,
                disabled = disableAdvancedEdit,
                unknownDefault = true
              ),
              FormLabel(htmlFor = "explicitRoi".refined)("ROI",
                                                         HelpIcon("configuration/roi.md".refined)
              ),
              customizableEnumSelect(
                id = "explicitRoi".refined,
                view = explicitRoi(props.scienceModeAdvanced),
                original = none,
                disabled = disableAdvancedEdit,
                exclude = obsoleteRois,
                unknownDefault = true
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
                postAddons = List("nm")
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
                  size = Button.Size.Small,
                  severity = Button.Severity.Secondary,
                  padding = Padding.Compact
                )("View Sequence")
              ),
              Button(
                size = Button.Size.Small,
                severity = Button.Severity.Secondary,
                padding = Padding.Compact,
                onClick = props.editState.set(ConfigEditState.TableView)
              )(Icons.ListIcon, "View Suggested Configs")(^.tpe := "button")
                .unless(isCustomized(props.scienceModeAdvanced)),
              Button(
                size = Button.Size.Small,
                severity = Button.Severity.Danger,
                padding = Padding.Compact,
                onClick = props.editState.set(ConfigEditState.DetailsView) >>
                  exposureModeEnum.set(none) >>
                  invalidateITC >>
                  revertCustomizations(props.scienceModeAdvanced)
              )(Icons.TrashUnstyled, "Revert Customizations")
                .when(isCustomized(props.scienceModeAdvanced)),
              Button(
                size = Button.Size.Small,
                severity = Button.Severity.Secondary,
                padding = Padding.Compact,
                onClick = props.editState.set(ConfigEditState.SimpleEdit)
              )(Icons.Edit, "Customize")
                .when(props.editState.get === ConfigEditState.DetailsView),
              Button(
                size = Button.Size.Small,
                severity = Button.Severity.Secondary,
                padding = Padding.Compact,
                onClick = props.editState.set(ConfigEditState.AdvancedEdit)
              )(
                Icons.ExclamationTriangle.clazz(ExploreStyles.WarningIcon),
                "Advanced Customization"
              )
                .when(props.editState.get === ConfigEditState.SimpleEdit)
            )
          )
      }
}

object AdvancedConfigurationPanel {
  sealed abstract class GmosAdvancedConfigurationPanel[
    T <: ScienceModeAdvanced,
    S <: ScienceModeBasic: Reusability,
    Input,
    Props <: AdvancedConfigurationPanel[T, S, Input],
    Grating: Enumerated: Display,
    Filter: Enumerated: Display,
    Fpu: Enumerated: Display
  ] extends AdvancedConfigurationPanelBuilder[
        T,
        S,
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
  final case class GmosNorthLongSlit(
    obsId:                    Observation.Id,
    title:                    String,
    subtitle:                 Option[NonEmptyString],
    scienceModeAdvanced:      Aligner[ScienceModeAdvanced.GmosNorthLongSlit,
                                 GmosNorthLongSlitAdvancedConfigInput
    ],
    scienceModeBasic:         ScienceModeBasic.GmosNorthLongSlit,
    spectroscopyRequirements: SpectroscopyRequirementsData,
    potITC:                   View[Pot[Option[ITCSuccess]]],
    editState:                View[ConfigEditState],
    confMatrix:               SpectroscopyModesMatrix
  )(implicit val ctx:         AppContextIO)
      extends ReactFnProps[AdvancedConfigurationPanel.GmosNorthLongSlit](
        AdvancedConfigurationPanel.GmosNorthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosNorthLongSlit,
        ScienceModeBasic.GmosNorthLongSlit,
        GmosNorthLongSlitAdvancedConfigInput,
      ]

  object GmosNorthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosNorthLongSlit,
        ScienceModeBasic.GmosNorthLongSlit,
        GmosNorthLongSlitAdvancedConfigInput,
        AdvancedConfigurationPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu,
      ] {

    @inline override protected def isCustomized(aligner: AA)(implicit ctx: AppContextIO): Boolean =
      aligner.get =!= ScienceModeAdvanced.GmosNorthLongSlit.Empty

    @inline override protected def revertCustomizations(aligner: AA)(implicit
      ctx:                                                       AppContextIO
    ): Callback =
      aligner.view(_.toInput).set(ScienceModeAdvanced.GmosNorthLongSlit.Empty)

    @inline override protected def overrideWavelength(aligner: AA)(implicit
      ctx:                                                     AppContextIO
    ): View[Option[Wavelength]] =
      aligner
        .zoom(ScienceModeAdvanced.GmosNorthLongSlit.overrideWavelength,
              GmosNorthLongSlitAdvancedConfigInput.overrideWavelength.modify
        )
        .view(_.map(_.toInput).orUnassign)

    @inline override protected def overrideGrating(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[GmosNorthGrating]] =
      aligner
        .zoom(ScienceModeAdvanced.GmosNorthLongSlit.overrideGrating,
              GmosNorthLongSlitAdvancedConfigInput.overrideGrating.modify
        )
        .view(_.orUnassign)

    @inline override protected def overrideFilter(aligner: AA)(implicit
      ctx:                                                 AppContextIO
    ): View[Option[GmosNorthFilter]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosNorthLongSlit.overrideFilter,
        GmosNorthLongSlitAdvancedConfigInput.overrideFilter.modify
      )
      .view(_.orUnassign)

    @inline override protected def overrideFpu(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[GmosNorthFpu]] = aligner
      .zoom(ScienceModeAdvanced.GmosNorthLongSlit.overrideFpu,
            GmosNorthLongSlitAdvancedConfigInput.overrideFpu.modify
      )
      .view(_.orUnassign)

    @inline override protected def overrideExposureTimeMode(aligner: AA)(implicit
      ctx:                                                           AppContextIO
    ): View[Option[ExposureTimeMode]] = aligner
      .zoom(ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode,
            GmosNorthLongSlitAdvancedConfigInput.overrideExposureTimeMode.modify
      )
      .view(_.map(_.toInput).orUnassign)

    private val explicitXBin =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitXBin
    private val explicitYBin =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitYBin

    private def binningAligner(
      aligner: AA
    ): Aligner[Option[(GmosXBinning, GmosYBinning)], GmosNorthLongSlitAdvancedConfigInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitXBin, explicitYBin), f => i => f(i))

    @inline override protected def explicitBinning(aligner: AA)(implicit
      ctx:                                                  AppContextIO
    ): View[Option[(GmosXBinning, GmosYBinning)]] =
      binningAligner(aligner)
        .viewMod { oxy =>
          val xy = oxy.unzip
          GmosNorthLongSlitAdvancedConfigInput.explicitXBin
            .replace(xy._1.orUnassign)
            .andThen(GmosNorthLongSlitAdvancedConfigInput.explicitYBin.replace(xy._2.orUnassign))
        }

    private val explicitReadMode =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitAmpReadMode
    private val explicitGain     =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthLongSlitAdvancedConfigInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    @inline override protected def explicitReadModeGain(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosNorthLongSlitAdvancedConfigInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthLongSlitAdvancedConfigInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    @inline override protected def explicitRoi(aligner: AA)(implicit
      ctx:                                              AppContextIO
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosNorthLongSlit.explicitRoi,
        GmosNorthLongSlitAdvancedConfigInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitWavelengthDithers(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[NonEmptyList[DitherNanoMeters]]] = aligner
      .zoom(ScienceModeAdvanced.GmosNorthLongSlit.explicitWavelengthDithers,
            GmosNorthLongSlitAdvancedConfigInput.explicitWavelengthDithersNm.modify
      )
      .view(_.map(_.map(_.value: BigDecimal).toList).orUnassign)

    @inline override protected def explicitSpatialOffsets(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosNorthLongSlit.explicitSpatialOffsets,
        GmosNorthLongSlitAdvancedConfigInput.explicitSpatialOffsets.modify _
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val gratingLens = ScienceModeBasic.GmosNorthLongSlit.grating
    @inline override protected val filterLens  = ScienceModeBasic.GmosNorthLongSlit.filter
    @inline override protected val fpuLens     = ScienceModeBasic.GmosNorthLongSlit.fpu

    @inline override protected val obsoleteGratings = GmosNorthGrating.all.filter(_.obsolete).toSet
    @inline override protected val obsoleteFilters  = GmosNorthFilter.all.filter(_.obsolete).toSet
  }

  // Gmos South Long Slit

  final case class GmosSouthLongSlit(
    obsId:                    Observation.Id,
    title:                    String,
    subtitle:                 Option[NonEmptyString],
    scienceModeAdvanced:      Aligner[ScienceModeAdvanced.GmosSouthLongSlit,
                                 GmosSouthLongSlitAdvancedConfigInput
    ],
    scienceModeBasic:         ScienceModeBasic.GmosSouthLongSlit,
    spectroscopyRequirements: SpectroscopyRequirementsData,
    potITC:                   View[Pot[Option[ITCSuccess]]],
    editState:                View[ConfigEditState],
    confMatrix:               SpectroscopyModesMatrix
  )(implicit val ctx:         AppContextIO)
      extends ReactFnProps[AdvancedConfigurationPanel.GmosSouthLongSlit](
        AdvancedConfigurationPanel.GmosSouthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosSouthLongSlit,
        ScienceModeBasic.GmosSouthLongSlit,
        GmosSouthLongSlitAdvancedConfigInput
      ]

  object GmosSouthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosSouthLongSlit,
        ScienceModeBasic.GmosSouthLongSlit,
        GmosSouthLongSlitAdvancedConfigInput,
        AdvancedConfigurationPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu,
      ] {

    @inline override protected def isCustomized(aligner: AA)(implicit ctx: AppContextIO): Boolean =
      aligner.get =!= ScienceModeAdvanced.GmosSouthLongSlit.Empty

    @inline override protected def revertCustomizations(aligner: AA)(implicit
      ctx:                                                       AppContextIO
    ): Callback =
      aligner.view(_.toInput).set(ScienceModeAdvanced.GmosSouthLongSlit.Empty)

    @inline override def overrideWavelength(aligner: AA)(implicit
      ctx:                                           AppContextIO
    ): View[Option[Wavelength]] =
      aligner
        .zoom(ScienceModeAdvanced.GmosSouthLongSlit.overrideWavelength,
              GmosSouthLongSlitAdvancedConfigInput.overrideWavelength.modify
        )
        .view(_.map(_.toInput).orUnassign)

    @inline override protected def overrideGrating(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[GmosSouthGrating]] =
      aligner
        .zoom(ScienceModeAdvanced.GmosSouthLongSlit.overrideGrating,
              GmosSouthLongSlitAdvancedConfigInput.overrideGrating.modify
        )
        .view(_.orUnassign)

    @inline override protected def overrideFilter(aligner: AA)(implicit
      ctx:                                                 AppContextIO
    ): View[Option[GmosSouthFilter]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosSouthLongSlit.overrideFilter,
        GmosSouthLongSlitAdvancedConfigInput.overrideFilter.modify
      )
      .view(_.orUnassign)

    @inline override protected def overrideFpu(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[GmosSouthFpu]] = aligner
      .zoom(ScienceModeAdvanced.GmosSouthLongSlit.overrideFpu,
            GmosSouthLongSlitAdvancedConfigInput.overrideFpu.modify
      )
      .view(_.orUnassign)

    @inline override protected def overrideExposureTimeMode(aligner: AA)(implicit
      ctx:                                                           AppContextIO
    ): View[Option[ExposureTimeMode]] = aligner
      .zoom(ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode,
            GmosSouthLongSlitAdvancedConfigInput.overrideExposureTimeMode.modify
      )
      .view(_.map(_.toInput).orUnassign)

    private val explicitXBin =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitXBin
    private val explicitYBin =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitYBin

    private def binningAligner(
      aligner: AA
    ): Aligner[Option[(GmosXBinning, GmosYBinning)], GmosSouthLongSlitAdvancedConfigInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitXBin, explicitYBin), f => i => f(i))

    @inline override protected def explicitBinning(aligner: AA)(implicit
      ctx:                                                  AppContextIO
    ): View[Option[(GmosXBinning, GmosYBinning)]] =
      binningAligner(aligner)
        .viewMod { oxy =>
          val xy = oxy.unzip
          GmosSouthLongSlitAdvancedConfigInput.explicitXBin
            .replace(xy._1.orUnassign)
            .andThen(GmosSouthLongSlitAdvancedConfigInput.explicitYBin.replace(xy._2.orUnassign))
        }

    private val explicitReadMode =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitAmpReadMode
    private val explicitGain     =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthLongSlitAdvancedConfigInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    @inline override protected def explicitReadModeGain(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosSouthLongSlitAdvancedConfigInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthLongSlitAdvancedConfigInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    @inline override protected def explicitRoi(aligner: AA)(implicit
      ctx:                                              AppContextIO
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosSouthLongSlit.explicitRoi,
        GmosSouthLongSlitAdvancedConfigInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    @inline override protected def explicitWavelengthDithers(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[NonEmptyList[DitherNanoMeters]]] = aligner
      .zoom(ScienceModeAdvanced.GmosSouthLongSlit.explicitWavelengthDithers,
            GmosSouthLongSlitAdvancedConfigInput.explicitWavelengthDithersNm.modify
      )
      .view(_.map(_.map(_.value: BigDecimal).toList).orUnassign)

    @inline override protected def explicitSpatialOffsets(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosSouthLongSlit.explicitSpatialOffsets,
        GmosSouthLongSlitAdvancedConfigInput.explicitSpatialOffsets.modify _
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline override protected val gratingLens = ScienceModeBasic.GmosSouthLongSlit.grating
    @inline override protected val filterLens  = ScienceModeBasic.GmosSouthLongSlit.filter
    @inline override protected val fpuLens     = ScienceModeBasic.GmosSouthLongSlit.fpu

    @inline override protected val obsoleteGratings = GmosSouthGrating.all.filter(_.obsolete).toSet
    @inline override protected val obsoleteFilters  = GmosSouthFilter.all.filter(_.obsolete).toSet
  }
}
