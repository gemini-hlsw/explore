// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import explore.components.CustomizableInputTextOptional
import explore.components.HelpIcon
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Offset
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.syntax.all.given

final case class OffsetsControl(
  view:         View[Option[NonEmptyList[Offset.Q]]],
  defaultValue: NonEmptyList[Offset.Q],
  onChange:     Callback,
  disabled:     Boolean
) extends ReactFnProps(OffsetsControl)

object OffsetsControl
    extends ReactFnComponent[OffsetsControl](props =>
      CustomizableInputTextOptional(
        id = "offsets".refined,
        value = props.view.withOnMod(_ => props.onChange),
        defaultValue = props.defaultValue,
        label = React.Fragment(
          "Spatial Offsets",
          HelpIcon("configuration/spatial-offsets.md".refined)
        ),
        validFormat = ExploreModelValidators.offsetQNELValidWedge,
        changeAuditor = ChangeAuditor
          .bigDecimal(integers = 3.refined, decimals = 2.refined)
          .toSequence()
          .optional,
        units = "arcsec".some,
        disabled = props.disabled
      )
    )
