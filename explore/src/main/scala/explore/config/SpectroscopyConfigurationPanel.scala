// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.AppCtx
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.SpectroscopyConfigurationOptions
import explore.model.enum.FocalPlaneOptions
import explore.model.enum.SpectroscopyCapabilities
import explore.model.formats._
import explore.model.reusability._
import explore.targeteditor.InputWithUnits
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Display
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

  implicit val specCapabDisplay: Display[SpectroscopyCapabilities]         = Display.by(_.label, _.label)
  implicit val focaLPlaneDisplay: Display[FocalPlaneOptions]               = Display.by(_.label, _.label)
  implicit val optionsReuse: Reusability[SpectroscopyConfigurationOptions] = Reusability.derive
  implicit val propsReuse: Reusability[Props]                              = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        AppCtx.using { implicit appCtx =>
          val wv                       = p.options.zoom(SpectroscopyConfigurationOptions.wavelength)
          val resolutionPower          = p.options.zoom(SpectroscopyConfigurationOptions.resolutionPower)
          val signalToNoise            = p.options.zoom(SpectroscopyConfigurationOptions.signalToNoise)
          val signalToNoiseAt          = p.options.zoom(SpectroscopyConfigurationOptions.signalToNoiseAt)
          val wavelengthRange          = p.options.zoom(SpectroscopyConfigurationOptions.wavelengthRange)
          val focalPlane               = p.options.zoom(SpectroscopyConfigurationOptions.focalPlane)
          val focalPlaneAngle          = p.options.zoom(SpectroscopyConfigurationOptions.focalPlaneAngle)
          val spectroscopyCapabilities =
            p.options.zoom(SpectroscopyConfigurationOptions.capabilities)

          ReactFragment(
            <.label("Wavelength",
                    HelpIcon("configuration/simple/wavelength.md"),
                    ExploreStyles.SkipToNext
            ),
            InputWithUnits(
              id = "configuration-wavelength",
              clazz = Css.Empty,
              inline = true,
              value = wv,
              units = "nm",
              validFormat = ValidFormatInput.fromFormatOptional(formatWavelength),
              changeAuditor = ChangeAuditor.fromFormat(formatWavelength).decimal(3).optional,
              disabled = false
            ),
            <.label("λ / Δλ",
                    HelpIcon("configuration/simple/spectral-resolution.md"),
                    ExploreStyles.SkipToNext
            ),
            FormInputEV(
              id = "configuration-resolution-power",
              value = resolutionPower,
              validFormat = ValidFormatInput.fromFormatOptional(formatPosInt),
              changeAuditor = ChangeAuditor.fromFormat(formatPosInt).optional
            ),
            <.label("S / N",
                    HelpIcon("configuration/simple/signal_to_noise.md"),
                    ExploreStyles.SkipToNext
            ),
            FormInputEV(id = "signal-to-noise",
                        value = signalToNoise,
                        validFormat = ValidFormatInput.fromFormatOptional(formatPosBigDecimal)
            ),
            <.div(
              ExploreStyles.SignalToNoiseAt,
              <.label("at"),
              InputWithUnits(
                id = "signal-to-noise-at",
                clazz = Css.Empty,
                value = signalToNoiseAt,
                units = "nm",
                validFormat = ValidFormatInput.fromFormatOptional(formatWavelength),
                changeAuditor = ChangeAuditor.fromFormat(formatWavelength).decimal(3).optional,
                disabled = false
              )
            ),
            <.label("λ Range",
                    HelpIcon("configuration/simple/frequency_range.md"),
                    ExploreStyles.SkipToNext
            ),
            InputWithUnits(
              id = "wavelength-range",
              clazz = Css.Empty,
              inline = true,
              value = wavelengthRange,
              units = "nm",
              validFormat = ValidFormatInput.fromFormatOptional(formatWavelength),
              changeAuditor = ChangeAuditor.fromFormat(formatWavelength).decimal(3).optional,
              disabled = false
            ),
            <.label("Focal Plane",
                    HelpIcon("configuration/simple/focal_plane.md"),
                    ExploreStyles.SkipToNext
            ),
            EnumViewOptionalSelect(id = "focal-plane",
                                   placeholder = "Focal plane",
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
                    HelpIcon("configuration/simple/capabilities.md"),
                    ExploreStyles.SkipToNext
            ),
            EnumViewOptionalSelect(
              id = "spectroscopy-capabilities",
              clazz = ExploreStyles.ConfigurationCapabilities,
              clearable = true,
              upward = true,
              placeholder = "Extra capablities",
              value = spectroscopyCapabilities
            )
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
