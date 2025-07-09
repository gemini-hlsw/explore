// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import explore.model.formats.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import org.typelevel.cats.time.given

import java.time.LocalTime

import scalajs.js

case class Time24HInputView(
  id:          String Refined NonEmpty,
  value:       View[LocalTime],
  label:       js.UndefOr[TagMod] = js.undefined,
  units:       js.UndefOr[String] = js.undefined,
  groupClass:  js.UndefOr[Css] = js.undefined,
  inputClass:  js.UndefOr[Css] = js.undefined,
  disabled:    Boolean = false,
  placeholder: js.UndefOr[String] = js.undefined
) extends ReactFnProps[Time24HInputView](Time24HInputView.component)

object Time24HInputView:
  private type Props = Time24HInputView

  private val component =
    ScalaFnComponent[Props] { props =>
      FormInputTextView(
        id = props.id,
        value = props.value,
        units = props.units,
        groupClass = props.groupClass,
        inputClass = props.inputClass,
        validFormat = time24h,
        changeAuditor = hmChangeAuditor.optional,
        label = props.label,
        placeholder = props.placeholder.getOrElse("HH:mm"),
        disabled = props.disabled
      )
    }
