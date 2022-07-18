// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.validation.InputValidFormat
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import react.common._
import react.common.implicits._
import react.semanticui._
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.input.IconPosition
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.LabelPointing

import scala.scalajs.js

final case class InputWithUnits[EV[_], A](
  value:           EV[A],
  validFormat:     InputValidFormat[A],
  changeAuditor:   ChangeAuditor,
  icon:            js.UndefOr[ShorthandSB[Icon]] = js.undefined,
  iconPosition:    js.UndefOr[IconPosition] = js.undefined,
  id:              NonEmptyString,
  label:           js.UndefOr[ShorthandS[Label]] = js.undefined,
  units:           TagMod,
  clazz:           Css = ExploreStyles.Grow(1),
  disabled:        Boolean = false,
  columnSpam:      Int Refined Interval.Closed[1, 16] = 2,
  inline:          js.UndefOr[Boolean] = js.undefined,
  size:            js.UndefOr[SemanticSize] = js.undefined
)(implicit val ev: ExternalValue[EV], val eq: Eq[A])
    extends ReactFnProps[InputWithUnits[Any, Any]](InputWithUnits.component)

object InputWithUnits {
  type Props[F[_], A] = InputWithUnits[F, A]

  implicit class InputWithUnitsOps[EV[_], A, B](val input: InputWithUnits[EV, Option[A]])
      extends AnyVal {
    def clearable(implicit ev: ExternalValue[EV], ev3: Eq[A]) =
      input.copy(icon = clearInputIcon[EV, A](input.value))

    // When an icon is added to a FormInputEV, SUI adds extra padding on the right to make
    // space for the icon. However, with some layouts this can cause resizing issues, so this
    // method removes that extra padding. See `clearInputIcon` for more details.
    def clearableNoPadding(implicit ev: ExternalValue[EV], ev3: Eq[A]) = {
      val newClazz = input.clazz |+| ExploreStyles.ClearableInputPaddingReset
      input.copy(icon = clearInputIcon[EV, A](input.value), clazz = newClazz)
    }
  }

  val component = componentF[Any, Any]

  def componentF[F[_], A] =
    ScalaFnComponent[Props[F, A]] { p =>
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
          inline = p.inline,
          icon = p.icon,
          iconPosition = p.iconPosition
        )(p.ev, p.eq),
        <.span(
          ExploreStyles.UnitsLabel,
          p.units
        )
      )
    }

}
