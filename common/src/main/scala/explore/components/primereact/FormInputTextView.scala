// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.primereact

import cats.*
import cats.data.NonEmptyChain
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.validation.*
import lucuma.ui.input.AuditResult
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability.*
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.document
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import react.common.*
import react.primereact.InputText
import react.primereact.PrimeStyles
import reactST.primereact.components.{Button => CButton}

import scala.scalajs.js
import scala.scalajs.js.|

import scalajs.js.JSConverters._

/**
 * FormInput component that uses a crystal View to share the content of the field
 */
final case class FormInputTextView[A](
  id:            NonEmptyString,
  label:         js.UndefOr[TagMod] = js.undefined,
  preAddons:     List[TagMod | CButton.Builder] = List.empty,
  postAddons:    List[TagMod | CButton.Builder] = List.empty,
  groupClass:    js.UndefOr[Css] = js.undefined,
  inputClass:    js.UndefOr[Css] = js.undefined,
  disabled:      js.UndefOr[Boolean] = js.undefined,
  error:         js.UndefOr[NonEmptyString] = js.undefined,
  placeholder:   js.UndefOr[String] = js.undefined,
  value:         View[A],
  validFormat:   InputValidFormat[A] = InputValidSplitEpi.id,
  changeAuditor: ChangeAuditor = ChangeAuditor.accept,
  onTextChange:  String => Callback = _ => Callback.empty,
  onValidChange: FormInputTextView.ChangeCallback[Boolean] = _ => Callback.empty,
  onBlur:        FormInputTextView.ChangeCallback[EitherErrors[A]] = (_: EitherErrors[A]) => Callback.empty
)(using val eq:  Eq[A])
    extends ReactFnProps[FormInputTextView[Any]](FormInputTextView.component) {
  def stringValue: String = validFormat.reverseGet(value.get)
}

object FormInputTextView {
  protected type Props[A]          = FormInputTextView[A]
  protected type ChangeCallback[A] = A => Callback

  // queries the dom based on id. Onus is on user to make id's unique.
  private def getInputElement(id: NonEmptyString): CallbackTo[Option[html.Input]] =
    CallbackTo(Option(document.querySelector(s"#${id.value}").asInstanceOf[html.Input]))

  protected def buildComponent[A] = {
    def audit(
      auditor:         ChangeAuditor,
      value:           String,
      setDisplayValue: String => Callback,
      inputElement:    Option[html.Input],
      setCursor:       Option[(Int, Int)] => Callback,
      lastKeyCode:     Int
    ): CallbackTo[String] = {
      val cursor: Option[(Int, Int)] = inputElement.map(i => (i.selectionStart, i.selectionEnd))

      def setStateCursorFromInput(offset: Int): Callback =
        setCursor(cursor.map { case (start, end) => (start + offset, end + offset) })

      val cursorOffsetForReject: Int =
        lastKeyCode match {
          case KeyCode.Backspace => 1
          case KeyCode.Delete    => 0
          case _                 => -1
        }

      val c = cursor match {
        case Some((start, _)) => start
        case _                => value.length
      }

      auditor.audit(value, c) match {
        case AuditResult.Accept                  =>
          setCursor(none) >> setDisplayValue(value).as(value)
        case AuditResult.NewString(newS, offset) =>
          setStateCursorFromInput(offset) >> setDisplayValue(newS).as(newS)
        case AuditResult.Reject                  =>
          setStateCursorFromInput(cursorOffsetForReject) >> CallbackTo(value)
      }

    }

    def validate(
      displayValue:  String,
      validFormat:   InputValidFormat[A],
      onValidChange: FormInputTextView.ChangeCallback[Boolean],
      cb:            EitherErrors[A] => Callback = _ => Callback.empty
    ): Callback = {
      val validated = validFormat.getValid(displayValue)
      onValidChange(validated.isRight) >> cb(validated)
    }

    ScalaFnComponent
      .withHooks[Props[A]]
      .useStateBy(props => props.stringValue)        // displayValue
      .useState(none[(Int, Int)])                    // cursor
      .useRef(0)                                     // lastKeyCode
      .useRef(none[html.Input])                      // inputElement
      .useState(none[NonEmptyChain[NonEmptyString]]) // errors
      .useEffectWithDepsBy((props, _, _, _, _, _) => props.stringValue)(
        (_, displayValue, _, _, _, errors) =>
          newValue => displayValue.setState(newValue) >> errors.setState(none)
      )
      .useEffectOnMountBy((props, displayValue, cursor, lastKeyCode, inputElement, _) =>
        getInputElement(props.id) >>= (element =>
          inputElement.set(element) >>
            audit(
              props.changeAuditor,
              props.stringValue,
              displayValue.setState,
              element,
              cursor.setState,
              lastKeyCode.value
            )
        ) >>= (value => validate(value, props.validFormat, props.onValidChange))
      )
      .useEffectBy((_, _, cursor, _, inputElement, _) =>
        (for {
          i <- inputElement.value
          c <- cursor.value
        } yield Callback(i.setSelectionRange(c._1, c._2))).orEmpty
      )
      .render { (props, displayValue, cursor, lastKeyCode, inputElement, errors) =>
        val errorChain: Option[NonEmptyChain[NonEmptyString]] =
          (props.error.toOption, errors.value) match {
            case (Some(a), Some(b)) => (a +: b).some
            case (None, Some(b))    => b.some
            case (Some(a), None)    => NonEmptyChain(a).some
            case (None, None)       => none
          }

        val error: Option[String] = errorChain.map(_.mkString_(", "))

        val onTextChange: ReactEventFrom[HTMLInputElement & Element] => Callback =
          (e: ReactEventFrom[HTMLInputElement & Element]) => {
            // Capture the value outside setState, react reuses the events
            val v = e.target.value
            audit(
              props.changeAuditor,
              v,
              displayValue.setState,
              inputElement.value,
              cursor.setState,
              lastKeyCode.value
            ).flatMap(newS =>
              // First update the internal state, then call the outside listener
              errors.setState(none) >>
                props.onTextChange(newS) >>
                validate(displayValue.value, props.validFormat, props.onValidChange)
            )
          }

        val submit: Callback =
          validate(
            displayValue.value,
            props.validFormat,
            props.onValidChange,
            { validated =>
              val validatedCB = validated match {
                case Right(a) =>
                  implicit val eq = props.eq

                  // Only set if resulting A changed.
                  if (props.value.get =!= a)
                    props.value.set(a)
                  else // A didn't change, but redisplay formatted string.
                    displayValue.setState(props.stringValue)
                case Left(e)  =>
                  errors.setState(e.some)
              }
              validatedCB >> props.onBlur(validated)
            }
          )

        val onKeyDown: ReactKeyboardEventFromInput => Callback = e =>
          // TODO keyCode can be undefined (despite the facade). This happens when selecting a value in form auto-fill.
          if (e.keyCode === KeyCode.Enter)
            submit
          else
            lastKeyCode.set(e.keyCode) >> cursor.setState(none)

        FormInputText(
          id = props.id,
          label = props.label,
          groupClass = props.groupClass,
          inputClass =
            error.map(_ => PrimeStyles.Invalid).orEmpty |+| props.inputClass.toOption.orEmpty,
          tooltip = error.map(s => s: VdomNode).orUndefined,
          disabled = props.disabled,
          preAddons = props.preAddons,
          postAddons = props.postAddons,
          onBlur = _ => submit,
          onChange = onTextChange,
          onKeyDown = onKeyDown,
          placeholder = props.placeholder,
          value = displayValue.value
        )
      }
  }

  protected val component = buildComponent[Any]
}
