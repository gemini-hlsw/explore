// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import coulomb.cats.implicits._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.SpectroscopyConfigurationOptions
import explore.model.display._
import explore.model.formats._
import explore.targeteditor.InputWithUnits
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._

final case class SpectroscopyConfigurationPanel(
  options: View[SpectroscopyConfigurationOptions]
) extends ReactProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel {
  type Props = SpectroscopyConfigurationPanel

  implicit val optionsReuse: Reusability[SpectroscopyConfigurationOptions] = Reusability.derive
  implicit val propsReuse: Reusability[Props]                              = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
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

        val wvMicroInput    = ValidFormatInput.fromFormatOptional(formatWavelengthMicron)
        val wvChangeAuditor = ChangeAuditor.fromFormat(formatWavelengthMicron).decimal(3).optional

        ReactFragment(
          <.label("Wavelength", HelpIcon("configuration/wavelength.md"), ExploreStyles.SkipToNext),
          InputWithUnits(
            id = "configuration-wavelength",
            clazz = Css.Empty,
            inline = true,
            value = wv,
            units = "μm",
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
            validFormat = ValidFormatInput.fromFormatOptional(formatPosInt),
            changeAuditor = ChangeAuditor.fromFormat(formatPosInt).optional
          ),
          <.label("S / N", HelpIcon("configuration/signal_to_noise.md"), ExploreStyles.SkipToNext),
          FormInputEV(
            id = "signal-to-noise",
            value = signalToNoise,
            validFormat = ValidFormatInput.fromFormatOptional(formatPosBigDecimal),
            changeAuditor = ChangeAuditor.fromFormat(formatPosBigDecimal).optional
          ),
          <.div(
            ExploreStyles.SignalToNoiseAt,
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
          <.label("Focal Plane",
                  HelpIcon("configuration/focal_plane.md"),
                  ExploreStyles.SkipToNext
          ),
          EnumViewOptionalSelect(id = "focal-plane",
                                 placeholder = "Any",
                                 upward = true,
                                 value = focalPlane,
                                 clearable = true
          ),
          <.div(
            ExploreStyles.SignalToNoiseAt,
            InputWithUnits(
              id = "spectroscopy-capabilities",
              clazz = Css.Empty,
              value = focalPlaneAngle,
              units = "arcsec",
              validFormat = ValidFormatInput.fromFormatOptional(formatArcsec),
              changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional,
              disabled = false
            )
          ),
          <.label("Capabilities",
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
      .configure(Reusability.shouldComponentUpdate)
      .build
}
