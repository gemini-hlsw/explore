// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.ReuseView
import crystal.react.View
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ScienceModeAdvanced
import explore.optics._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum._
import lucuma.core.model.Observation
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.implicits._
import lucuma.ui.reusability._
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._

sealed trait AdvancedConfigurationPanel[T <: ScienceModeAdvanced] {
  val obsId: Observation.Id
  val scienceMode: Reuse[View[T]]
  val onShowBasic: Reuse[Callback]

  implicit val ctx: AppContextIO
}

sealed abstract class AdvancedConfigurationPanelBuilder[
  T <: ScienceModeAdvanced,
  Props <: AdvancedConfigurationPanel[T],
  Grating: Enumerated,
  Filter: Enumerated,
  Fpu: Enumerated,
  XBinning: Enumerated,
  YBinning: Enumerated,
  ReadMode: Enumerated,
  Gain: Enumerated,
  Roi: Enumerated
] {
  protected implicit val reuseProps: Reusability[Props]

  @inline protected val overrideGratingLens: Lens[T, Option[Grating]]
  @inline protected val overrideFilterLens: Lens[T, Option[Filter]]
  @inline protected val overrideFpuLens: Lens[T, Option[Fpu]]
  // @inline protected val nodAndShuffleLens: Option[Lens[T, Option[GmosNodAndShuffle]]] // Left on purpose
  @inline protected val explicitXBin: Lens[T, Option[XBinning]]
  @inline protected val explicitYBin: Lens[T, Option[YBinning]]
  @inline protected val explicitReadMode: Lens[T, Option[ReadMode]]
  @inline protected val explicitGain: Lens[T, Option[Gain]]
  @inline protected val explicitRoi: Lens[T, Option[Roi]]

  protected implicit val displayBinning: Display[(XBinning, YBinning)]
  protected implicit val displayReadModeGain: Display[(ReadMode, Gain)]

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .renderWithReuse { props =>
        implicit val ctx = props.ctx

        Form(size = Small)(
          ExploreStyles.Compact,
          ExploreStyles.AdvancedConfigurationGrid
        )(
          <.div(
            ExploreStyles.ExploreForm,
            ExploreStyles.AdvancedConfigurationCol1
          )(
            <.label("Grating", HelpIcon("configuration/grating.md")),
            EnumViewOptionalSelect[ReuseView, Grating](
              id = "override-grating",
              value = props.scienceMode.zoom(overrideGratingLens)
            ),
            <.label("Filter", HelpIcon("configuration/filter.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect[ReuseView, Filter](
              id = "override-filter",
              value = props.scienceMode.zoom(overrideFilterLens)
            ),
            <.label("FPU", HelpIcon("configuration/fpu.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect[ReuseView, Fpu](
              id = "override-fpu",
              value = props.scienceMode.zoom(overrideFpuLens)
            )
          ),
          <.div(ExploreStyles.ExploreForm, ExploreStyles.AdvancedConfigurationCol2)(
            <.label("Binning", HelpIcon("configuration/binning.md")),
            EnumViewOptionalSelect[ReuseView, (XBinning, YBinning)](
              id = "explicitXBin",
              value = props.scienceMode.zoom(unsafeDisjointOptionZip(explicitXBin, explicitYBin))
            ),
            <.label("Read Mode", HelpIcon("configuration/read-mode.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect[ReuseView, (ReadMode, Gain)](
              id = "explicitReadMode",
              value =
                props.scienceMode.zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain))
            ),
            <.label("ROI", HelpIcon("configuration/roi.md"), ExploreStyles.SkipToNext),
            EnumViewOptionalSelect[ReuseView, Roi](
              id = "explicitRoi",
              value = props.scienceMode.zoom(explicitRoi)
            )
          ),
          <.div(ExploreStyles.ExploreForm, ExploreStyles.AdvancedConfigurationCol3)(
          ),
          <.div(ExploreStyles.AdvancedConfigurationButtons)(
            SequenceEditorPopup(
              props.obsId,
              trigger = Reuse.by(props.obsId)(
                Button(
                  size = Small,
                  compact = true,
                  clazz = ExploreStyles.VeryCompact,
                  content = "View Sequence"
                )
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

  // Gmos North Long Slit
  final case class GmosNorthLongSlit(
    obsId:            Observation.Id,
    scienceMode:      Reuse[View[ScienceModeAdvanced.GmosNorthLongSlit]],
    onShowBasic:      Reuse[Callback]
  )(implicit val ctx: AppContextIO)
      extends ReactFnProps[AdvancedConfigurationPanel.GmosNorthLongSlit](
        AdvancedConfigurationPanel.GmosNorthLongSlit.component
      )
      with AdvancedConfigurationPanel[ScienceModeAdvanced.GmosNorthLongSlit]

  sealed abstract class GmosAdvancedConfigurationPanel[
    T <: ScienceModeAdvanced,
    Props <: AdvancedConfigurationPanel[T],
    Grating: Enumerated,
    Filter: Enumerated,
    Fpu: Enumerated
  ] extends AdvancedConfigurationPanelBuilder[
        T,
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
    protected implicit val displayBinning: Display[(GmosXBinning, GmosYBinning)] =
      Display.by(
        { case (x, y) => s"${x.shortName} x ${y.shortName}" },
        { case (x, y) => s"${x.longName} x ${y.longName}" }
      )

    protected implicit val displayReadModeGain: Display[(GmosAmpReadMode, GmosAmpGain)] =
      Display.by( // Shortname is in lower case for some reason
        { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
        { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
      )

  }

  object GmosNorthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosNorthLongSlit,
        AdvancedConfigurationPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu,
      ] {
    override implicit protected lazy val reuseProps: Reusability[GmosNorthLongSlit] =
      Reusability.derive

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
  }

  // Gmos South Long Slit

  final case class GmosSouthLongSlit(
    obsId:            Observation.Id,
    scienceMode:      Reuse[View[ScienceModeAdvanced.GmosSouthLongSlit]],
    onShowBasic:      Reuse[Callback]
  )(implicit val ctx: AppContextIO)
      extends ReactFnProps[AdvancedConfigurationPanel.GmosSouthLongSlit](
        AdvancedConfigurationPanel.GmosSouthLongSlit.component
      )
      with AdvancedConfigurationPanel[ScienceModeAdvanced.GmosSouthLongSlit]

  object GmosSouthLongSlit
      extends GmosAdvancedConfigurationPanel[
        ScienceModeAdvanced.GmosSouthLongSlit,
        AdvancedConfigurationPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu,
      ] {
    override implicit protected lazy val reuseProps: Reusability[GmosSouthLongSlit] =
      Reusability.derive

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
  }
}
