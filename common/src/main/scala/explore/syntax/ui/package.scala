// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax

import cats.Eq
import cats.syntax.all._
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.utils._
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import org.scalajs.dom.Window
import react.common.GenericFnComponentPA
import react.common.Css
import react.common.implicits._

import scala.scalajs.js.UndefOr
import scala.scalajs.js
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.TagMod.apply
import japgolly.scalajs.react.vdom.html_<^._

package object ui {
  implicit class WindowOps(val self: Window) extends AnyVal {
    def canFitTwoPanels: Boolean =
      self.innerWidth <= Constants.TwoPanelCutoff
  }

  implicit class FormInputEVOps[EV[_], A, B](val input: FormInputEV[EV, Option[A]]) extends AnyVal {
    def clearable(implicit ev: ExternalValue[EV], ev3: Eq[A]) =
      input.copy(icon = clearInputIcon[EV, A](input.value))

    // When an icon is added to a FormInputEV, SUI adds extra padding on the right to make
    // space for the icon. However, with some layouts this can cause resizing issues, so this
    // method removes that extra padding. See `clearInputIcon` for more details.
    def clearableNoPadding(implicit ev: ExternalValue[EV], ev3: Eq[A]) = {
      val newClazz: UndefOr[Css] =
        input.clazz.fold(ExploreStyles.ClearableInputPaddingReset)(
          _ |+| ExploreStyles.ClearableInputPaddingReset
        )
      input.copy(icon = clearInputIcon[EV, A](input.value), clazz = newClazz)
    }
  }

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

  type FnPA[P <: js.Object] = GenericFnComponentPA[P, ?]
  given Conversion[FnPA[?], UndefOr[VdomNode]] = _.render
  given Conversion[FnPA[?], VdomNode]          = _.render

  given Conversion[Css, TagMod] =
    ^.className := _.htmlClass
}
