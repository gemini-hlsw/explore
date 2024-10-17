// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.FocalPlane
import lucuma.core.math.Wavelength
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormEnumDropdownOptionalView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SpectroscopyConfigurationPanel(
  options:  View[ScienceRequirements.Spectroscopy],
  readonly: Boolean,
  units:    WavelengthUnits
) extends ReactFnProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel extends ConfigurationFormats:
  private type Props = SpectroscopyConfigurationPanel

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(
        FocalPlane.SingleSlit.some.widen[FocalPlane]
      ) // For now only SlitView is allowed
      .render: (p, fpView) =>
        val prevSignalToNoiseAt = p.options.get.signalToNoiseAt

        // Set SignalToNoiseAt to wavelength if it is empty
        val options = p.options.withOnMod(s =>
          if (s.wavelength =!= prevSignalToNoiseAt && prevSignalToNoiseAt.isEmpty)
            p.options.set(s.copy(signalToNoiseAt = s.wavelength))
          else Callback.empty
        )

        val resolution             = options.zoom(ScienceRequirements.Spectroscopy.resolution)
        val wv                     = options.zoom(ScienceRequirements.Spectroscopy.wavelength)
        val wavelengthDelta        = options.zoom(ScienceRequirements.Spectroscopy.wavelengthCoverage)
        val focalPlaneAngle        = options.zoom(ScienceRequirements.Spectroscopy.focalPlaneAngle)
        val spectroscopyCapability =
          options.zoom(ScienceRequirements.Spectroscopy.capability)

        ReactFragment(
          FormInputTextView[View, Option[Wavelength]](
            id = "configuration-wavelength".refined,
            value = wv,
            label = <.div(
              "Wavelength",
              HelpIcon("configuration/wavelength.md".refined)
            ),
            groupClass = ExploreStyles.WarningInput.when_(wv.get.isEmpty),
            postAddons = wv.get.fold(List(requiredForITC))(_ => Nil),
            units = p.units.symbol,
            validFormat = p.units.toInputWedge,
            changeAuditor = p.units.toAuditor,
            disabled = p.readonly
          ).clearable(^.autoComplete.off),
          FormInputTextView(
            id = "configuration-resolution-power".refined,
            value = resolution,
            label = ReactFragment(
              "λ / Δλ",
              HelpIcon("configuration/spectral_resolution.md".refined)
            ),
            validFormat = InputValidSplitEpi.posInt.optional,
            changeAuditor = ChangeAuditor.posInt.optional,
            disabled = p.readonly
          ).clearable(^.autoComplete.off),
          SignalToNoiseAt(options, p.readonly, p.units),
          FormInputTextView(
            id = "wavelength-range".refined,
            value = wavelengthDelta,
            units = p.units.symbol,
            label = ReactFragment(
              "Δλ",
              HelpIcon("configuration/wavelength_coverage.md".refined)
            ),
            validFormat = p.units.toDeltaInputWedge,
            changeAuditor = p.units.toAuditor,
            disabled = p.readonly
          ).clearable(^.autoComplete.off),
          FormLabel("focal-plane".refined)("Focal Plane",
                                           HelpIcon("configuration/focal_plane.md".refined)
          ),
          <.div(
            LucumaPrimeStyles.FormField |+| ExploreStyles.BasicConfigurationFocalPlane,
            FormEnumDropdownOptionalView(
              id = "focal-plane".refined,
              placeholder = "Any",
              value = fpView,
              disabled = true
            ),
            FormInputTextView(
              id = "focal-plane-angle".refined,
              value = focalPlaneAngle,
              units = "arcsec",
              validFormat = slitLengthFormat,
              changeAuditor = slitLengthChangeAuditor,
              disabled = p.readonly
            ).clearable(^.autoComplete.off)
          ),
          FormEnumDropdownOptionalView(
            id = "spectroscopy-capability".refined,
            placeholder = "None",
            value = spectroscopyCapability,
            label = ReactFragment(
              "Capability",
              HelpIcon("configuration/capability.md".refined)
            ),
            disabled = true
          )
        )
