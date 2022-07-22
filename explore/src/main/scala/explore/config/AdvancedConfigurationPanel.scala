// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import clue.data.syntax._
import coulomb.Quantity
import coulomb.refined._
import crystal.Pot
import crystal.react.View
import crystal.react.hooks._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.common.ObsQueries._
import explore.common.ScienceQueries._
import explore.components.HelpIcon
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.config.ExposureTimeModeType._
import explore.implicits._
import explore.model.DitherNanoMeters
import explore.model.ExploreModelValidators
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.display._
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.SpectroscopyModeRow
import explore.optics._
import explore.syntax.ui._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums._
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.NonNegDuration
import lucuma.core.model.Observation
import lucuma.core.syntax.all._
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation._
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability._
import monocle.Lens
import queries.schemas.implicits._
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.collections.form.FormInput
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._
import spire.math.Bounded
import spire.math.Interval

import java.time.Duration
import scala.scalajs.js.JSConverters._

sealed trait AdvancedConfigurationPanel[T <: ScienceModeAdvanced, S <: ScienceModeBasic, Input] {
  val obsId: Observation.Id
  val title: String
  val subtitle: Option[NonEmptyString]
  val scienceModeAdvanced: Aligner[T, Input]
  val scienceModeBasic: S
  val spectroscopyRequirements: SpectroscopyRequirementsData
  val potITC: View[Pot[Option[ITCSuccess]]]
  val onShowBasic: Callback

  implicit val ctx: AppContextIO
}

sealed abstract class AdvancedConfigurationPanelBuilder[
  T <: ScienceModeAdvanced,
  S <: ScienceModeBasic: Eq,
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

  // Note: truncates to Int.MaxValue - shouldn't have durations longer than that...
  private def durationToSeconds(nnd: NonNegDuration): NonNegInt =
    NonNegInt.unsafeFrom(math.min(nnd.value.toMicros / 1000L / 1000L, Int.MaxValue.toLong).toInt)

  private def secondsToDuration(secs: NonNegInt): NonNegDuration =
    NonNegDuration.unsafeFrom(secs.value.toLong.seconds)

  private def optExposureTimeModeToExpTimeSecs(oetm: Option[ExposureTimeMode]): Option[NonNegInt] =
    oetm.flatMap(etm => ExposureTimeMode.exposureTime.getOption(etm)).map(durationToSeconds)

  val itcNoneMsg = "No ITC Results"

  val wavelengthChangeAuditor =
    ChangeAuditor
      .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
      .allow(s => s === "0" || s === "0.")
      .decimal(3)

  implicit val reuseS: Reusability[S] = Reusability.byEq

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

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy { props =>
        implicit val ctx = props.ctx
        overrideExposureTimeMode(props.scienceModeAdvanced).get
          .map(ExposureTimeModeType.fromExposureTimeMode)
      }
      .useStateViewBy { (props, _) =>
        implicit val ctx = props.ctx
        val oetm         = overrideExposureTimeMode(props.scienceModeAdvanced).get
        optExposureTimeModeToExpTimeSecs(oetm)
      }
      .useEffectWithDepsBy { (props, _, _) =>
        implicit val ctx = props.ctx
        optExposureTimeModeToExpTimeSecs(overrideExposureTimeMode(props.scienceModeAdvanced).get)
      }((_, _, expTimeOverride) =>
        newExpTime =>
          if (newExpTime === expTimeOverride.get) Callback.empty
          else expTimeOverride.set(newExpTime)
      )
      // filter the spectroscopy matrix by the requirements that don't get overridden
      // by the advanced config (wavelength, for example).
      .useMemoBy((props, _, _) =>
        (props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.capabilities,
         props.spectroscopyRequirements.focalPlaneAngle,
         props.spectroscopyRequirements.resolution,
         props.spectroscopyRequirements.wavelengthCoverage
        )
      ) { (props, _, _) =>
        { case (fp, cap, fpa, res, cov) =>
          props.ctx.staticData.spectroscopyMatrix.filtered(
            focalPlane = fp,
            capabilities = cap,
            slitWidth = fpa,
            resolution = res,
            coverage = cov.map(_.micrometer.toValue[BigDecimal].toRefined[Positive])
          )
        }
      }
      // Try to find the readonly data from the spectroscopy matrix
      .useMemoBy { (props, _, _, rows) =>
        implicit val ctx = props.ctx
        val advanced     = props.scienceModeAdvanced
        (props.scienceModeBasic,
         props.spectroscopyRequirements.wavelength,
         rows,
         overrideWavelength(advanced).get,
         overrideGrating(advanced).get,
         overrideFilter(advanced).get,
         overrideFpu(advanced).get
        )
      } { (props, _, _, _) =>
        { case (basic, reqsWavelength, rows, _, _, _, _) =>
          findMatrixData(basic, props.scienceModeAdvanced.get, reqsWavelength, rows)
        }
      }
      .render {
        (
          props,
          exposureModeEnum,
          expTimeOverrideSecs,
          _,
          readonlyData
        ) =>
          implicit val ctx = props.ctx

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

          def dithersControl(onChange: Callback): VdomElement =
            ReactFragment(
              <.label("λ Dithers", HelpIcon("configuration/lambda-dithers.md")),
              InputWithUnits(
                id = "dithers",
                value =
                  explicitWavelengthDithers(props.scienceModeAdvanced).withOnMod(_ => onChange),
                validFormat = ExploreModelValidators.dithersValidSplitEpi,
                changeAuditor =
                  ChangeAuditor.bigDecimal(integers = 3, decimals = 1).toSequence().optional,
                units = "nm"
              ).clearable
            )

          def offsetsControl(onChange: Callback): VdomElement =
            ReactFragment(
              <.label("Spatial Offsets", HelpIcon("configuration/spatial-offsets.md")),
              InputWithUnits(
                id = "offsets",
                value = explicitSpatialOffsets(props.scienceModeAdvanced).withOnMod(_ => onChange),
                validFormat = ExploreModelValidators.offsetQNELValidWedge,
                changeAuditor =
                  ChangeAuditor.bigDecimal(integers = 3, decimals = 2).toSequence().optional,
                units = "arcsec"
              ).clearable
            )

          val invalidateITC: Callback =
            props.potITC.set(Pot.pending[Option[ITCSuccess]])

          val zeroDuration: NonNegDuration = NonNegDuration.unsafeFrom(Duration.ofMillis(0))

          def onModeMod(modType: Option[ExposureTimeModeType]): Callback = {
            val optITC: Option[ITCSuccess] = props.potITC.get.toOption.flatten
            val oetm                       = modType.map {
              case ExposureTimeModeType.SignalToNoise =>
                val sn = signalToNoiseView
                  .map(_.get)
                  .orElse(optITC.map(_.signalToNoise))
                  .getOrElse(PosBigDecimal.unsafeFrom(BigDecimal(100)))
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

          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.AdvancedConfigurationGrid
          )(
            <.div(
              ExploreStyles.ExploreForm,
              ExploreStyles.AdvancedConfigurationCol1
            )(
              <.label("Grating", HelpIcon("configuration/grating.md")),
              EnumViewOptionalSelect(
                id = "override-grating",
                value = overrideGrating(props.scienceModeAdvanced),
                clearable = true,
                placeholder = gratingLens.get(props.scienceModeBasic).shortName
              ),
              <.label("Filter", HelpIcon("configuration/filter.md"), ExploreStyles.SkipToNext),
              EnumViewOptionalSelect(
                id = "override-filter",
                value = overrideFilter(props.scienceModeAdvanced),
                clearable = true,
                placeholder = filterLens.get(props.scienceModeBasic).map(_.shortName).orUndefined
              ),
              <.label("Wavelength",
                      HelpIcon("configuration/wavelength.md"),
                      ExploreStyles.SkipToNext
              ),
              InputWithUnits(
                id = "override-wavelength",
                value = overrideWavelength(props.scienceModeAdvanced).withOnMod(_ => invalidateITC),
                units = "μm",
                validFormat = ExploreModelValidators.wavelengthValidWedge.optional,
                changeAuditor = wavelengthChangeAuditor.optional
              ).clearable,
              <.label("FPU", HelpIcon("configuration/fpu.md"), ExploreStyles.SkipToNext),
              EnumViewOptionalSelect(
                id = "override-fpu",
                value = overrideFpu(props.scienceModeAdvanced),
                clearable = true,
                placeholder = fpuLens.get(props.scienceModeBasic).shortName
              )
            ),
            <.div(ExploreStyles.ExploreForm, ExploreStyles.AdvancedConfigurationCol2)(
              <.label("Binning", HelpIcon("configuration/binning.md")),
              EnumViewOptionalSelect(
                id = "explicitXBin",
                value = explicitBinning(props.scienceModeAdvanced),
                clearable = true
              ),
              <.label("Read Mode",
                      HelpIcon("configuration/read-mode.md"),
                      ExploreStyles.SkipToNext
              ),
              EnumViewOptionalSelect(
                id = "explicitReadMode",
                value = explicitReadModeGain(props.scienceModeAdvanced),
                clearable = true
              ),
              <.label("ROI", HelpIcon("configuration/roi.md"), ExploreStyles.SkipToNext),
              EnumViewOptionalSelect(
                id = "explicitRoi",
                value = explicitRoi(props.scienceModeAdvanced),
                clearable = true
              ),
              <.label("λ / Δλ", ExploreStyles.SkipToNext),
              FormInput(
                value = readonlyData.value.fold("Unknown")(_.resolution.toString),
                disabled = true
              ),
              <.label("λ Coverage", ExploreStyles.SkipToNext),
              ReactFragment(
                FormInput(value = readonlyData.value.fold("Unknown")(_.formattedCoverage),
                          disabled = true
                ),
                <.span(ExploreStyles.UnitsLabel, "nm")
              )
            ),
            <.div(ExploreStyles.ExploreForm, ExploreStyles.AdvancedConfigurationCol3)(
              dithersControl(Callback.empty),
              offsetsControl(Callback.empty),
              <.label("Exposure Mode",
                      HelpIcon("configuration/exposure-mode.md"),
                      ExploreStyles.SkipToNext
              ),
              EnumViewOptionalSelect(
                id = "exposureMode",
                value = exposureModeEnum.withOnMod(onModeMod _),
                clearable = true
              ),
              <.label("S/N",
                      HelpIcon("configuration/signal-to-noise.md"),
                      ExploreStyles.SkipToNext
              ),
              signalToNoiseView
                .map(v =>
                  FormInputEV(
                    id = "signalToNoise",
                    value = v.withOnMod(_ => invalidateITC),
                    validFormat = InputValidWedge.truncatedPosBigDecimal(0),
                    changeAuditor = ChangeAuditor.posInt
                  ): VdomNode
                )
                .getOrElse(
                  potRender[Option[PosBigDecimal]](
                    valueRender = osn => {
                      val value = osn.fold(itcNoneMsg)(sn =>
                        InputValidWedge.truncatedPosBigDecimal(0).reverseGet(sn)
                      )
                      FormInput(value = value, disabled = true)
                    },
                    pendingRender = _ =>
                      <.div(ExploreStyles.InputReplacementIcon, Icons.Spinner.spin(true)): VdomNode
                  )(props.potITC.get.map(_.map(_.signalToNoise)))
                ),
              <.label("Exposure Time",
                      HelpIcon("configuration/exposure-time.md"),
                      ExploreStyles.SkipToNext
              ),
              expTimeOverrideSecs
                .mapValue((v: View[NonNegInt]) =>
                  InputWithUnits(
                    id = "exposureTime",
                    value = v
                      .withOnMod(secs =>
                        exposureTimeView.foldMap(_.set(secondsToDuration(secs))) >> invalidateITC
                      ),
                    validFormat = InputValidSplitEpi.refinedInt[NonNegative],
                    changeAuditor = ChangeAuditor.refinedInt[NonNegative](),
                    units = "sec"
                  ): TagMod
                )
                .getOrElse(
                  potRender[Option[NonNegDuration]](
                    valueRender = ot => {
                      val value =
                        ot.fold(itcNoneMsg)(t => durationToSeconds(t).toString)
                      ReactFragment(FormInput(value = value, disabled = true),
                                    <.span(ExploreStyles.UnitsLabel, "sec")
                      )
                    },
                    pendingRender = _ =>
                      <.div(ExploreStyles.InputReplacementIcon, Icons.Spinner.spin(true)): VdomNode
                  )(props.potITC.get.map(_.map(_.exposureTime)))
                ),
              <.label("Exposure Count",
                      HelpIcon("configuration/exposure-count.md"),
                      ExploreStyles.SkipToNext
              ),
              exposureCountView
                .map(v =>
                  FormInputEV(
                    id = "exposureCount",
                    value = v.withOnMod(_ => invalidateITC),
                    validFormat = InputValidSplitEpi.refinedInt[NonNegative],
                    changeAuditor = ChangeAuditor.refinedInt[NonNegative]()
                  ): TagMod
                )
                .getOrElse(
                  potRender[Option[NonNegInt]](
                    valueRender = oe => {
                      val value = oe.fold(itcNoneMsg)(_.toString)
                      FormInput(value = value, disabled = true)
                    },
                    pendingRender = _ =>
                      <.div(ExploreStyles.InputReplacementIcon, Icons.Spinner.spin(true)): VdomNode
                  )(props.potITC.get.map(_.map(_.exposures)))
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
                  size = Small,
                  compact = true,
                  clazz = ExploreStyles.VeryCompact,
                  content = "View Sequence"
                )(^.tpe := "button")
              ),
              Button(
                size = Small,
                compact = true,
                clazz = ExploreStyles.VeryCompact,
                content = "Simple Configuration",
                icon = Icons.ChevronsLeft,
                onClick = props.onShowBasic.value
              )(^.tpe := "button")
            )
          )
      }
}

object AdvancedConfigurationPanel {
  sealed abstract class GmosAdvancedConfigurationPanel[
    T <: ScienceModeAdvanced,
    S <: ScienceModeBasic: Eq,
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
      ]

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
    onShowBasic:              Callback
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

    @inline protected def overrideWavelength(aligner: AA)(implicit
      ctx:                                            AppContextIO
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
      .view(_.map(_.map(_.value).toList).orUnassign)

    @inline override protected def explicitSpatialOffsets(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosNorthLongSlit.explicitSpatialOffsets,
        GmosNorthLongSlitAdvancedConfigInput.explicitSpatialOffsets.modify _
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline protected val gratingLens = ScienceModeBasic.GmosNorthLongSlit.grating
    @inline protected val filterLens  = ScienceModeBasic.GmosNorthLongSlit.filter
    @inline protected val fpuLens     = ScienceModeBasic.GmosNorthLongSlit.fpu

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
    onShowBasic:              Callback
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

    @inline protected def overrideWavelength(aligner: AA)(implicit
      ctx:                                            AppContextIO
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
      .view(_.map(_.map(_.value).toList).orUnassign)

    @inline override protected def explicitSpatialOffsets(
      aligner:      AA
    )(implicit ctx: AppContextIO): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ScienceModeAdvanced.GmosSouthLongSlit.explicitSpatialOffsets,
        GmosSouthLongSlitAdvancedConfigInput.explicitSpatialOffsets.modify _
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    @inline protected val gratingLens = ScienceModeBasic.GmosSouthLongSlit.grating
    @inline protected val filterLens  = ScienceModeBasic.GmosSouthLongSlit.filter
    @inline protected val fpuLens     = ScienceModeBasic.GmosSouthLongSlit.fpu

  }
}
