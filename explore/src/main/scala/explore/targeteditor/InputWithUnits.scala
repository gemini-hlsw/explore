// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import react.common._
import react.semanticui._
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.LabelPointing

import scala.scalajs.js

final case class InputWithUnits[EV[_], A](
  value:           EV[A],
  validFormat:     ValidFormatInput[A],
  changeAuditor:   ChangeAuditor[A],
  id:              NonEmptyString,
  label:           js.UndefOr[ShorthandS[Label]] = js.undefined,
  units:           String,
  clazz:           Css = ExploreStyles.Grow(1),
  disabled:        Boolean = false,
  columnSpam:      Int Refined Interval.Closed[1, 16] = 2,
  inline:          js.UndefOr[Boolean] = js.undefined,
  size:            js.UndefOr[SemanticSize] = js.undefined
)(implicit val ev: ExternalValue[EV], val eq: Eq[A])
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
            changeAuditor = p.changeAuditor,
            clazz = p.clazz,
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            disabled = p.disabled,
            size = p.size,
            inline = p.inline
          )(p.ev, p.eq),
          <.span(
            ExploreStyles.UnitsLabel,
            p.units
          )
        )
      }
      .build

}
