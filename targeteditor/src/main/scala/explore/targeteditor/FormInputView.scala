// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.scalajs.js
import scala.scalajs.js.|

import cats.implicits._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEventFromInput
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import monocle.macros.Lenses
import react.common._
import react.semanticui._
import react.semanticui.collections.form.FormInput
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.input._
import react.semanticui.elements.label._
import crystal.ViewF
import crystal.react.implicits._
import gpp.ui.forms.InputOptics
import gpp.ui.forms.InputEV
import cats.effect.Effect

/**
  * FormInput component that uses a ViewF to share the content of the field
  */
final case class FormInputView[F[_], A](
  name:                String,
  id:                  String,
  action:              js.UndefOr[ShorthandSB[VdomNode]] = js.undefined,
  actionPosition:      js.UndefOr[ActionPosition] = js.undefined,
  as:                  js.UndefOr[AsC] = js.undefined,
  className:           js.UndefOr[String] = js.undefined,
  clazz:               js.UndefOr[Css] = js.undefined,
  content:             js.UndefOr[ShorthandS[VdomNode]] = js.undefined,
  control:             js.UndefOr[String] = js.undefined,
  disabled:            js.UndefOr[Boolean] = js.undefined,
  error:               js.UndefOr[ShorthandB[Label]] = js.undefined,
  fluid:               js.UndefOr[Boolean] = js.undefined,
  focus:               js.UndefOr[Boolean] = js.undefined,
  icon:                js.UndefOr[ShorthandSB[Icon]] = js.undefined,
  iconPosition:        js.UndefOr[IconPosition] = js.undefined,
  inline:              js.UndefOr[Boolean] = js.undefined,
  input:               js.UndefOr[VdomNode] = js.undefined,
  inverted:            js.UndefOr[Boolean] = js.undefined,
  label:               js.UndefOr[ShorthandS[Label]] = js.undefined,
  labelPosition:       js.UndefOr[LabelPosition] = js.undefined,
  loading:             js.UndefOr[Boolean] = js.undefined,
  required:            js.UndefOr[Boolean] = js.undefined,
  size:                js.UndefOr[SemanticSize] = js.undefined,
  tabIndex:            js.UndefOr[String | JsNumber] = js.undefined,
  tpe:                 js.UndefOr[String] = js.undefined,
  transparent:         js.UndefOr[Boolean] = js.undefined,
  width:               js.UndefOr[SemanticWidth] = js.undefined,
  view:                ViewF[F, A],
  optic:               InputOptics[A] = InputOptics.id,
  onChange:            FormInputView.ChangeCallback[A] =
    (_: A) => Callback.empty, // callback for parents of this component
  onBlur:              FormInputView.ChangeCallback[A] = (_: A) => Callback.empty
)(implicit val effect: Effect[F])
    extends ReactProps[FormInputView[Any, Any]](FormInputView.component) {
  def valGet: String = optic.reverseGet(view.get)
  def valSet(s: String): Callback = optic.getOption(s).map(a => view.set(a).runInCB).getOrEmpty
  val onBlurC: InputEV.ChangeCallback[String]   =
    (s: String) => optic.getOption(s).map(onBlur).getOrEmpty
  val onChangeC: InputEV.ChangeCallback[String] =
    (s: String) => optic.getOption(s).map(onChange).getOrEmpty
}

object FormInputView {
  type Props[F[_], A]    = FormInputView[F, A]
  type ChangeCallback[A] = A => Callback
  type Scope[F[_], A]    = RenderScope[Props[F, A], State, Unit]

  @Lenses
  final case class State(curValue: String, prevValue: String)

  def onTextChange[F[_], A]($ : Scope[F, A]): ReactEventFromInput => Callback =
    (e: ReactEventFromInput) => {
      // Capture the value outside setState, react reuses the events
      val v = e.target.value
      // First update the internal state, then call the outside listener
      $.setStateL(State.curValue)(v) *>
        // Next 2 might not be called if the InputOptics returns None
        $.props.valSet(v) *>
        $.props.onChangeC(v)
    }

  def onBlur[F[_], A]($ : Scope[F, A], c: ChangeCallback[String]): Callback =
    c($.state.curValue)

  protected val component =
    ScalaComponent
      .builder[Props[Any, Any]]
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        val newValue = props.valGet
        // Force new value from props if the prop changes (or we are initializing).
        stateOpt match {
          case Some(state) if newValue === state.prevValue => state
          case _                                           => State(newValue, newValue)
        }
      }
      .render { $ =>
        val p = $.props
        val s = $.state
        FormInput(
          p.action,
          p.actionPosition,
          p.as,
          p.className,
          p.clazz,
          p.content,
          p.control,
          p.disabled,
          p.error,
          p.fluid,
          p.focus,
          p.icon,
          p.iconPosition,
          p.inline,
          p.input,
          p.inverted,
          p.label,
          p.labelPosition,
          p.loading,
          js.undefined,
          onTextChange($),
          p.required,
          p.size,
          p.tabIndex,
          p.tpe,
          p.transparent,
          p.width,
          s.curValue
        )(^.id := p.id, ^.onBlur --> onBlur($, p.onBlurC))
      }
      .build
}
