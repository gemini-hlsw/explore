// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import explore.components.ui.ExploreStyles
import explore.model.display.*
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.FormLabel
import lucuma.ui.syntax.all.given

final case class LambdaAndIntervalFormValues(
  modeData:          Option[ModeData],
  centralWavelength: Wavelength,
  units:             WavelengthUnits
) extends ReactFnProps(LambdaAndIntervalFormValues)

object LambdaAndIntervalFormValues
    extends ReactFnComponent[LambdaAndIntervalFormValues](props =>
      val adjustedInterval =
        props.modeData.flatMap(md => md.wavelengthInterval(props.centralWavelength))

      given Display[BoundedInterval[Wavelength]] = wavelengthIntervalDisplay(props.units)

      React.Fragment(
        FormLabel(htmlFor = "lambda".refined)("λ / Δλ"),
        <.label(^.id := "lambda",
                ExploreStyles.FormValue,
                s"${props.modeData.fold("Unknown")(_.resolution.toString)}"
        ),
        FormLabel(htmlFor = "lambdaInterval".refined)("λ Interval"),
        <.label(^.id := "lambdaInterval",
                ExploreStyles.FormValue,
                s"${adjustedInterval.fold("Unknown")(_.shortName)} ${props.units.symbol}"
        )
      )
    )
