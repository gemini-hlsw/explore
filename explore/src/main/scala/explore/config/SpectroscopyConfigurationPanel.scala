// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.ExploreModelValidators
import explore.model.display.given
import explore.model.formats.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormEnumDropdownOptionalView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import explore.model.ScienceRequirements

case class SpectroscopyConfigurationPanel(
  options: View[ScienceRequirements.Spectroscopy]
) extends ReactFnProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel {
  type Props = SpectroscopyConfigurationPanel

  protected val component =
    ScalaFnComponent[Props] { p =>
      val wv                     = p.options.zoom(ScienceRequirements.Spectroscopy.wavelength)
      val resolution             = p.options.zoom(ScienceRequirements.Spectroscopy.resolution)
      val signalToNoise          = p.options.zoom(ScienceRequirements.Spectroscopy.signalToNoise)
      val signalToNoiseAt        = p.options.zoom(ScienceRequirements.Spectroscopy.signalToNoiseAt)
      val wavelengthDelta        =
        p.options.zoom(ScienceRequirements.Spectroscopy.wavelengthCoverage)
      val focalPlane             = p.options.zoom(ScienceRequirements.Spectroscopy.focalPlane)
      val focalPlaneAngle        = p.options.zoom(ScienceRequirements.Spectroscopy.focalPlaneAngle)
      val spectroscopyCapability =
        p.options.zoom(ScienceRequirements.Spectroscopy.capability)

      val wvMicroInput    = ExploreModelValidators.wavelengthValidWedge.optional
      val wvcMicroInput   = ExploreModelValidators.wavelengthDeltaValidWedge.optional
      val wvChangeAuditor = ChangeAuditor
        .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
        .allow(s => s === "0" || s === "0.")
        .decimal(3.refined)
        .optional

      ReactFragment(
        FormInputTextView[View, Option[Wavelength]](
          id = "configuration-wavelength".refined,
          value = wv,
          label = ReactFragment(
            "Wavelength",
            HelpIcon("configuration/wavelength.md".refined)
          ),
          groupClass = ExploreStyles.WarningInput.when_(wv.get.isEmpty),
          postAddons = wv.get.fold(List(requiredForITC))(_ => Nil),
          units = "μm",
          validFormat = wvMicroInput,
          changeAuditor = wvChangeAuditor
        ).clearable(^.autoComplete.off),
        FormInputTextView(
          id = "configuration-resolution-power".refined,
          value = resolution,
          label = ReactFragment(
            "λ / Δλ",
            HelpIcon("configuration/spectral_resolution.md".refined)
          ),
          validFormat = InputValidSplitEpi.posInt.optional,
          changeAuditor = ChangeAuditor.posInt.optional
        ).clearable(^.autoComplete.off),
        FormLabel("signal-to-noise".refined)(
          "S / N",
          HelpIcon("configuration/signal_to_noise.md".refined)
        ),
        <.div(
          LucumaStyles.FormField |+| ExploreStyles.BasicConfigurationSNAt,
          FormInputTextView(
            id = "signal-to-noise".refined,
            value = signalToNoise,
            groupClass = ExploreStyles.WarningInput.when_(signalToNoise.get.isEmpty),
            validFormat = ExploreModelValidators.signalToNoiseValidSplitEpi.optional,
            postAddons = signalToNoise.get.fold(List(requiredForITC))(_ => Nil),
            changeAuditor = ChangeAuditor.posBigDecimal(1.refined).optional
          ).withMods(^.autoComplete.off),
          FormLabel("signal-to-noise-at".refined)("at"),
          FormInputTextView(
            id = "signal-to-noise-at".refined,
            value = signalToNoiseAt,
            units = "μm",
            validFormat = wvMicroInput,
            changeAuditor = wvChangeAuditor
          ).clearable(^.autoComplete.off)
        ),
        FormInputTextView(
          id = "wavelength-range".refined,
          value = wavelengthDelta,
          units = "μm",
          label = ReactFragment(
            "Δλ",
            HelpIcon("configuration/wavelength_coverage.md".refined)
          ),
          validFormat = wvcMicroInput,
          changeAuditor = wvChangeAuditor
        ).clearable(^.autoComplete.off),
        FormLabel("focal-plane".refined)("Focal Plane",
                                         HelpIcon("configuration/focal_plane.md".refined)
        ),
        <.div(
          LucumaStyles.FormField |+| ExploreStyles.BasicConfigurationFocalPlane,
          FormEnumDropdownOptionalView(
            id = "focal-plane".refined,
            placeholder = "Any",
            value = focalPlane
          ),
          FormInputTextView(
            id = "focal-plane-angle".refined,
            value = focalPlaneAngle,
            units = "arcsec",
            validFormat = InputValidWedge.fromFormat(formatArcsec).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional
          ).clearable(^.autoComplete.off)
        ),
        FormEnumDropdownOptionalView(
          id = "spectroscopy-capability".refined,
          placeholder = "None",
          value = spectroscopyCapability,
          label = ReactFragment(
            "Capability",
            HelpIcon("configuration/capability.md".refined)
          )
        )
      )
    }
}
