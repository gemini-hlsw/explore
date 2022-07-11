// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import coulomb.cats.implicits._
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.SpectroscopyConfigurationOptions
import explore.model.display._
import explore.model.formats._
import explore.targeteditor.InputWithUnits
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.validation._
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import react.common._

final case class SpectroscopyConfigurationPanel(
  options: View[SpectroscopyConfigurationOptions]
) extends ReactFnProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel {
  type Props = SpectroscopyConfigurationPanel

  protected val component =
    ScalaFnComponent[Props] { p =>
      val wv                       = p.options.zoom(SpectroscopyConfigurationOptions.wavelengthQ)
      val resolution               = p.options.zoom(SpectroscopyConfigurationOptions.resolution)
      val signalToNoise            = p.options.zoom(SpectroscopyConfigurationOptions.signalToNoise)
      val signalToNoiseAt          = p.options.zoom(SpectroscopyConfigurationOptions.signalToNoiseAtQ)
      val wavelengthCoverage       =
        p.options.zoom(SpectroscopyConfigurationOptions.wavelengthCoverageQ)
      val focalPlane               = p.options.zoom(SpectroscopyConfigurationOptions.focalPlane)
      val focalPlaneAngle          = p.options.zoom(SpectroscopyConfigurationOptions.focalPlaneAngle)
      val spectroscopyCapabilities =
        p.options.zoom(SpectroscopyConfigurationOptions.capabilities)

      val wvMicroInput    = InputValidWedge.fromFormat(formatMicron).optional
      val wvChangeAuditor = ChangeAuditor.fromFormat(formatMicron).decimal(3).optional

      val wvUnits =
        <.span("μm ", requiredForITC.unless(wv.get.isDefined))

      ReactFragment(
        <.label("Wavelength", HelpIcon("configuration/wavelength.md"), ExploreStyles.SkipToNext),
        InputWithUnits(
          id = "configuration-wavelength",
          clazz = ExploreStyles.WarningInput.when_(wv.get.isEmpty),
          inline = true,
          value = wv,
          units = wvUnits,
          validFormat = wvMicroInput,
          changeAuditor = wvChangeAuditor,
          disabled = false
        ),
        <.label("λ / Δλ",
                HelpIcon("configuration/spectral_resolution.md"),
                ExploreStyles.SkipToNext
        ),
        FormInputEV(
          id = "configuration-resolution-power",
          value = resolution,
          validFormat = InputValidSplitEpi.posInt.optional,
          changeAuditor = ChangeAuditor.posInt.optional
        ),
        <.label("S / N", HelpIcon("configuration/signal_to_noise.md"), ExploreStyles.SkipToNext),
        FormInputEV(
          id = "signal-to-noise",
          value = signalToNoise,
          clazz = ExploreStyles.WarningInput.when_(signalToNoise.get.isEmpty),
          validFormat = InputValidSplitEpi.posBigDecimal.optional,
          changeAuditor = ChangeAuditor.posBigDecimal().optional
        ),
        <.div(
          ExploreStyles.InputWithLabel,
          requiredForITC.unless(signalToNoise.get.isDefined),
          <.label("at"),
          InputWithUnits(
            id = "signal-to-noise-at",
            clazz = Css.Empty,
            value = signalToNoiseAt,
            units = "μm",
            validFormat = wvMicroInput,
            changeAuditor = wvChangeAuditor,
            disabled = false
          )
        ),
        <.label("λ Coverage",
                HelpIcon("configuration/wavelength_coverage.md"),
                ExploreStyles.SkipToNext
        ),
        InputWithUnits(
          id = "wavelength-coverage",
          clazz = Css.Empty,
          inline = true,
          value = wavelengthCoverage,
          units = "μm",
          validFormat = wvMicroInput,
          changeAuditor = wvChangeAuditor,
          disabled = false
        ),
        <.label("Focal Plane", HelpIcon("configuration/focal_plane.md"), ExploreStyles.SkipToNext),
        EnumViewOptionalSelect(
          id = "focal-plane",
          placeholder = "Any",
          upward = true,
          value = focalPlane,
          clearable = true
        ),
        <.div(
          ExploreStyles.InputWithLabel,
          InputWithUnits(
            id = "spectroscopy-capabilities",
            clazz = Css.Empty,
            value = focalPlaneAngle,
            units = "arcsec",
            validFormat = InputValidWedge.fromFormat(formatArcsec).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional,
            disabled = false
          )
        ),
        <.label(
          "Capabilities",
          HelpIcon("configuration/capabilities.md"),
          ExploreStyles.SkipToNext
        ),
        EnumViewOptionalSelect(
          id = "spectroscopy-capabilities",
          clazz = ExploreStyles.ConfigurationCapabilities,
          clearable = true,
          upward = true,
          placeholder = "None",
          value = spectroscopyCapabilities
        )
      )
    }
}
