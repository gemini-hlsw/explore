// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.implicits.*
import crystal.react.View
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.BroadBand
import explore.model.ImagingConfigurationOptions
import explore.model.NarrowBand
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.CheckboxView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class ImagingConfigurationPanel(
  options:         View[ImagingConfigurationOptions],
  readonly:        Boolean,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps(ImagingConfigurationPanel)

object ImagingConfigurationPanel
    extends ReactFnComponent[ImagingConfigurationPanel](p =>

      val fov           = p.options.zoom(ImagingConfigurationOptions.fov)
      val signalToNoise = p.options.zoom(ImagingConfigurationOptions.signalToNoise)
      val narrowBand    =
        p.options.zoom(ImagingConfigurationOptions.narrowBand.andThen(NarrowBand.Value))
      val broadBand     = p.options.zoom(ImagingConfigurationOptions.broadBand.andThen(BroadBand.Value))

      ReactFragment(
        FormInputTextView(
          id = "configuration-fov".refined,
          value = fov,
          label = ReactFragment("Minimum FoV", HelpIcon("configuration/fov.md".refined)),
          units = "arcsec",
          validFormat = angleArcsecsFormat,
          changeAuditor = angleArcsecondsChangeAuditor,
          disabled = p.readonly
        ),
        <.div(
          ExploreStyles.ImagingFilterFilters,
          <.label("Filters",
                  HelpIcon("configuration/filter.md".refined),
                  LucumaPrimeStyles.FormFieldLabel
          ),
          CheckboxView(
            id = "narrowband-filter".refined,
            value = narrowBand,
            label = "Narrow",
            disabled = p.readonly
          ),
          CheckboxView(
            id = "broadband-filter".refined,
            value = broadBand,
            label = "Broad",
            disabled = p.readonly
          )
        ),
        SignalToNoiseInput(
          signalToNoise,
          p.calibrationRole,
          p.readonly
        )
      )
    )
