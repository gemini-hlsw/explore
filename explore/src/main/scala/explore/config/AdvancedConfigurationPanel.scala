// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.DitherNanoMeters
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.validators._
import explore.optics._
import explore.targeteditor.InputWithUnits
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum._
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.syntax.all._
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.scalajs.js.JSConverters._
import japgolly.scalajs.react.feature.ReactFragment

sealed trait AdvancedConfigurationPanel[T <: ScienceModeAdvanced, S <: ScienceModeBasic] {
  val obsId: Observation.Id
  val title: String
  val subtitle: Option[NonEmptyString]
  val scienceModeAdvanced: View[T]
  val scienceModeBasic: S
  val onShowBasic: Callback

  implicit val ctx: AppContextIO
}

sealed abstract class AdvancedConfigurationPanelBuilder[
  T <: ScienceModeAdvanced,
  S <: ScienceModeBasic,
  Props <: AdvancedConfigurationPanel[T, S],
  Grating: Enumerated: Display,
  Filter: Enumerated: Display,
  Fpu: Enumerated: Display,
  XBinning: Enumerated: Display,
  YBinning: Enumerated: Display,
  ReadMode: Enumerated: Display,
  Gain: Enumerated: Display,
  Roi: Enumerated: Display
] {
  @inline protected val overrideGratingLens: Lens[T, Option[Grating]]
  @inline protected val overrideFilterLens: Lens[T, Option[Filter]]
  @inline protected val overrideFpuLens: Lens[T, Option[Fpu]]
  // @inline protected val nodAndShuffleLens: Option[Lens[T, Option[GmosNodAndShuffle]]] // Left on purpose
  @inline protected val explicitXBin: Lens[T, Option[XBinning]]
  @inline protected val explicitYBin: Lens[T, Option[YBinning]]
  @inline protected val explicitReadMode: Lens[T, Option[ReadMode]]
  @inline protected val explicitGain: Lens[T, Option[Gain]]
  @inline protected val explicitRoi: Lens[T, Option[Roi]]
  @inline protected val explicitWavelengthDithers: Lens[T, Option[NonEmptyList[DitherNanoMeters]]]
  @inline protected val explicitSpatialOffsets: Lens[T, Option[NonEmptyList[Offset.Q]]]

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

  val dithersChangeAuditor: ChangeAuditor[Option[NonEmptyList[DitherNanoMeters]]] =
    ChangeAuditor.bigDecimal(3, 1).as[DitherNanoMeters].toSequence[NonEmptyList]().optional

  val offsetsChangeAuditor: ChangeAuditor[Option[NonEmptyList[Offset.Q]]] =
    ChangeAuditor.bigDecimal(3, 2).as[Offset.Q].toSequence[NonEmptyList]().optional

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        implicit val ctx = props.ctx

        val explicitWavelengthDithersView =
          props.scienceModeAdvanced.zoom(explicitWavelengthDithers)
        val explicitSpatialOffsetsView    = props.scienceModeAdvanced.zoom(explicitSpatialOffsets)

        val seqGenParameters: VdomElement =
          ReactFragment(
            <.label("λ Dithers", HelpIcon("configuration/lambda-dithers.md")),
            InputWithUnits(
              id = "dithers",
              value = explicitWavelengthDithersView,
              validFormat = dithersValidFormat,
              changeAuditor = dithersChangeAuditor,
              units = "nm"
            ),
            <.label("Spatial Offsets", HelpIcon("configuration/spatial-offsets.md")),
            InputWithUnits(
              id = "offsets",
              value = explicitSpatialOffsetsView,
              validFormat = offsetQNELValidFormat,
              changeAuditor = offsetsChangeAuditor,
              units = "nm"
            )
          )

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
              value = props.scienceModeAdvanced.zoom(overrideGratingLens),
              clearable = true,
              placeholder = gratingLens.get(props.scienceModeBasic).shortName
            ),
            <.label("Filter", HelpIcon("configuration/filter.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect(
              id = "override-filter",
              value = props.scienceModeAdvanced.zoom(overrideFilterLens),
              clearable = true,
              placeholder = filterLens.get(props.scienceModeBasic).map(_.shortName).orUndefined
            ),
            <.label("FPU", HelpIcon("configuration/fpu.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect(
              id = "override-fpu",
              value = props.scienceModeAdvanced.zoom(overrideFpuLens),
              clearable = true,
              placeholder = fpuLens.get(props.scienceModeBasic).shortName
            )
          ),
          <.div(ExploreStyles.ExploreForm, ExploreStyles.AdvancedConfigurationCol2)(
            <.label("Binning", HelpIcon("configuration/binning.md")),
            EnumViewOptionalSelect(
              id = "explicitXBin",
              value =
                props.scienceModeAdvanced.zoom(unsafeDisjointOptionZip(explicitXBin, explicitYBin)),
              clearable = true
            ),
            <.label("Read Mode", HelpIcon("configuration/read-mode.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect(
              id = "explicitReadMode",
              value = props.scienceModeAdvanced
                .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain)),
              clearable = true
            ),
            <.label("ROI", HelpIcon("configuration/roi.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect(
              id = "explicitRoi",
              value = props.scienceModeAdvanced.zoom(explicitRoi),
              clearable = true
            )
          ),
          <.div(ExploreStyles.ExploreForm, ExploreStyles.AdvancedConfigurationCol3)(
            seqGenParameters
          ),
          <.div(ExploreStyles.AdvancedConfigurationButtons)(
            SequenceEditorPopup(
              props.obsId,
              props.title,
              props.subtitle,
              seqGenParameters,
              trigger = Button(
                size = Small,
                compact = true,
                clazz = ExploreStyles.VeryCompact,
                content = "View Sequence"
              )
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
    S <: ScienceModeBasic,
    Props <: AdvancedConfigurationPanel[T, S],
    Grating: Enumerated,
    Filter: Enumerated,
    Fpu: Enumerated
  ] extends AdvancedConfigurationPanelBuilder[
        T,
        S,
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
    obsId:               Observation.Id,
    title:               String,
    subtitle:            Option[NonEmptyString],
    scienceModeAdvanced: View[ScienceModeAdvanced.GmosNorthLongSlit],
    scienceModeBasic:    ScienceModeBasic.GmosNorthLongSlit,
    onShowBasic:         Callback
  )(implicit val ctx:    AppContextIO)
      extends ReactFnProps[AdvancedConfigurationPanel.GmosNorthLongSlit](
        AdvancedConfigurationPanel.GmosNorthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosNorthLongSlit,
        ScienceModeBasic.GmosNorthLongSlit
      ]

  object GmosNorthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosNorthLongSlit,
        ScienceModeBasic.GmosNorthLongSlit,
        AdvancedConfigurationPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu,
      ] {
    @inline override protected lazy val overrideGratingLens =
      ScienceModeAdvanced.GmosNorthLongSlit.overrideGrating
    @inline override protected lazy val overrideFilterLens  =
      ScienceModeAdvanced.GmosNorthLongSlit.overrideFilter
    @inline override protected lazy val overrideFpuLens     =
      ScienceModeAdvanced.GmosNorthLongSlit.overrideFpu
    @inline protected val explicitXBin                      =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitXBin
    @inline protected val explicitYBin                      =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitYBin
    @inline protected val explicitReadMode                  =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitAmpReadMode
    @inline protected val explicitGain                      =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitAmpGain
    @inline protected val explicitRoi                       =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitRoi
    @inline protected val explicitWavelengthDithers         =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitWavelengthDithers
    @inline protected val explicitSpatialOffsets            =
      ScienceModeAdvanced.GmosNorthLongSlit.explicitSpatialOffsets

    @inline protected val gratingLens = ScienceModeBasic.GmosNorthLongSlit.grating
    @inline protected val filterLens  = ScienceModeBasic.GmosNorthLongSlit.filter
    @inline protected val fpuLens     = ScienceModeBasic.GmosNorthLongSlit.fpu

  }

  // Gmos South Long Slit

  final case class GmosSouthLongSlit(
    obsId:               Observation.Id,
    title:               String,
    subtitle:            Option[NonEmptyString],
    scienceModeAdvanced: View[ScienceModeAdvanced.GmosSouthLongSlit],
    scienceModeBasic:    ScienceModeBasic.GmosSouthLongSlit,
    onShowBasic:         Callback
  )(implicit val ctx:    AppContextIO)
      extends ReactFnProps[AdvancedConfigurationPanel.GmosSouthLongSlit](
        AdvancedConfigurationPanel.GmosSouthLongSlit.component
      )
      with AdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosSouthLongSlit,
        ScienceModeBasic.GmosSouthLongSlit
      ]

  object GmosSouthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosSouthLongSlit,
        ScienceModeBasic.GmosSouthLongSlit,
        AdvancedConfigurationPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu,
      ] {
    @inline override protected lazy val overrideGratingLens =
      ScienceModeAdvanced.GmosSouthLongSlit.overrideGrating
    @inline override protected lazy val overrideFilterLens  =
      ScienceModeAdvanced.GmosSouthLongSlit.overrideFilter
    @inline override protected lazy val overrideFpuLens     =
      ScienceModeAdvanced.GmosSouthLongSlit.overrideFpu
    @inline protected val explicitXBin                      =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitXBin
    @inline protected val explicitYBin                      =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitYBin
    @inline protected val explicitReadMode                  =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitAmpReadMode
    @inline protected val explicitGain                      =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitAmpGain
    @inline protected val explicitRoi                       =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitRoi
    @inline protected val explicitWavelengthDithers         =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitWavelengthDithers
    @inline protected val explicitSpatialOffsets            =
      ScienceModeAdvanced.GmosSouthLongSlit.explicitSpatialOffsets

    @inline protected val gratingLens = ScienceModeBasic.GmosSouthLongSlit.grating
    @inline protected val filterLens  = ScienceModeBasic.GmosSouthLongSlit.filter
    @inline protected val fpuLens     = ScienceModeBasic.GmosSouthLongSlit.fpu
  }
}
