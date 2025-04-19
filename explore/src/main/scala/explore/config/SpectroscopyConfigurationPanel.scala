// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.Instrument
import lucuma.core.math.Wavelength
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SpectroscopyConfigurationPanel(
  instrument:      Option[Instrument],
  options:         View[ScienceRequirements.Spectroscopy],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel extends ConfigurationFormats:
  private type Props = SpectroscopyConfigurationPanel

  protected val component =
    ScalaFnComponent[Props]: p =>
      useStateView(
        FocalPlane.SingleSlit.some.widen[FocalPlane]
      ).map: fpView => // For now only SlitView is allowed

        val exposureTimeMode       = p.options.zoom(ScienceRequirements.Spectroscopy.exposureTimeMode)
        val resolution             = p.options.zoom(ScienceRequirements.Spectroscopy.resolution)
        val wv                     = p.options.zoom(ScienceRequirements.Spectroscopy.wavelength)
        val wavelengthDelta        = p.options.zoom(ScienceRequirements.Spectroscopy.wavelengthCoverage)
        val focalPlaneAngle        = p.options.zoom(ScienceRequirements.Spectroscopy.focalPlaneAngle)
        val spectroscopyCapability =
          p.options.zoom(ScienceRequirements.Spectroscopy.capability)

        val focalPlaneSupported    = false
        val capabililitesSupported = false
        ReactFragment(
          FormInputTextView[View, Option[Wavelength]](
            id = "configuration-wavelength".refined,
            value = wv,
            label = <.div(
              "Wavelength",
              HelpIcon("configuration/wavelength.md".refined)
            ),
            groupClass = ExploreStyles.WarningInput.when_(wv.get.isEmpty),
            postAddons = wv.get.fold(List(p.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
            units = p.units.symbol,
            validFormat = p.units.toInputWedge,
            changeAuditor = p.units.toAuditor,
            disabled = p.readonly
          ).clearable(^.autoComplete.off),
          FormInputTextView(
            id = "configuration-resolution-power".refined,
            value = resolution,
            label = ReactFragment(
              "Min Resolution",
              HelpIcon("configuration/spectral_resolution.md".refined)
            ),
            validFormat = InputValidSplitEpi.posInt.optional,
            changeAuditor = ChangeAuditor.posInt.optional,
            disabled = p.readonly
          ).clearable(^.autoComplete.off),
          FormInputTextView(
            id = "wavelength-range".refined,
            value = wavelengthDelta,
            units = p.units.symbol,
            label = ReactFragment(
              "Min λ Coverage",
              HelpIcon("configuration/wavelength_coverage.md".refined)
            ),
            validFormat = p.units.toDeltaInputWedge,
            changeAuditor = p.units.toAuditor,
            disabled = p.readonly
          ).clearable(^.autoComplete.off),
          ExposureTimeModeEditor(p.instrument,
                                 wv.get,
                                 exposureTimeMode,
                                 p.readonly,
                                 p.units,
                                 p.calibrationRole
          ),
          Option.when(focalPlaneSupported) { // Hide until supported
            ReactFragment(
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
              )
            )
          },
          Option.when(capabililitesSupported)( // Hide until supported
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
        )
