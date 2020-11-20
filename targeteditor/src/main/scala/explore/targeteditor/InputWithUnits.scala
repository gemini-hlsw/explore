// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.effect.Effect
import crystal.ViewF
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ValidFormatInput
import react.common._
import react.semanticui.elements.label.LabelPointing

final case class InputWithUnits[F[_]: Effect, A](
  value:           ViewF[F, A],
  validFormat:     ValidFormatInput[A],
  id:              NonEmptyString,
  label:           String,
  units:           String,
  disabled:        ViewF[F, Boolean],
  columnSpam:      Int Refined Interval.Closed[1, 16] = 2
)(implicit val ev: ExternalValue[ViewF[F, *]], val eq: Eq[A])
    extends ReactProps[InputWithUnits[Any, Any]](InputWithUnits.component) {}

object InputWithUnits {
  type Props[F[_], A] = InputWithUnits[F, A]

  val component = componentF[Any, Any]

  def componentF[F[_], A] =
    ScalaComponent
      .builder[Props[F, A]]
      .render_P { p =>
        React.Fragment(
          FormInputEV(
            id = p.id,
            label = p.label,
            value = p.value,
            validFormat = p.validFormat,
            clazz = ExploreStyles.Grow(1),
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            disabled = p.disabled.get
          )(p.ev, p.eq),
          <.div(
            ExploreStyles.UnitsLabel,
            p.units
          )
        )
      }
      .build

}
