// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats._
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms._
import lucuma.ui.optics._
import react.common.style.Css
import react.semanticui.elements.button.Button
import reactST.reactTable.facade.cell._

object ReactTableHelpers {

  /**
   * Create a component which edits the value in a FormInputEv and can be passed to the TableDef
   * componentColumn method.
   *
   * @param lens
   *   Lens to go between the Row value and the Cell value.
   * @param validFormat
   *   The ValidFormatInput for the editor.
   * @param changeAuditor
   *   The ChangeAuditor for the editor.
   * @param disabled
   *   Whether editing should be disabled or not.
   * @return
   *   The component.
   */
  def editableViewColumn[A](
    validFormat:   ValidFormatInput[A],
    changeAuditor: ChangeAuditor[A] = ChangeAuditor.accept[A],
    disabled:      Boolean = false,
    modifiers:     Seq[TagMod] = Seq.empty
  )(implicit eq:   Eq[A]) =
    ScalaComponent
      .builder[View[A]]
      .render_P { view =>
        FormInputEV(
          id = newId,
          value = view,
          validFormat = validFormat,
          changeAuditor = changeAuditor,
          disabled = disabled,
          modifiers = modifiers
        )
      }
      .build
      .cmapCtorProps[CellProps[_, View[A], _]](_.value)
      .toJsComponent
      .raw

  /**
   * Create a component which edits the value via a EnumViewSelect and can be passed to the TableDef
   * componentColumn method.
   *
   * @param lens
   *   Lens to go between the Row value and the Cell value.
   * @param disabled
   *   Whether editing should be disabled or not.
   * @param excludeFn
   *   An optional function that determines a set of enums thst should be excluded from the
   *   dropdown.
   * @return
   *   The component.
   */
  def editableEnumViewColumn[A](
    disabled:            Boolean = false,
    excludeFn:           Option[A => Set[A]] = None,
    modifiers:           Seq[TagMod] = Seq.empty
  )(implicit enumerated: Enumerated[A], display: Display[A]) =
    ScalaComponent
      .builder[View[A]]
      .render_P { view =>
        val excluded = excludeFn.fold(Set.empty[A])(_.apply(view.get))

        EnumViewSelect(
          id = newId,
          value = view,
          exclude = excluded,
          compact = true,
          disabled = disabled,
          modifiers = modifiers
        )
      }
      .build
      .cmapCtorProps[CellProps[_, View[A], _]](_.value)
      .toJsComponent
      .raw

  /**
   * Create a component containing a button which can be passed to the TableDef componentColumn
   * method. The button is passed in to allow maximum flexibility, but the onClick handler should
   * not be specified for the button, since it will be added here.
   *
   * The [A] type is the type of the entire data row.
   *
   * @param button
   *   The button to put in the cell.
   * @param onClick
   *   The onClick handler for the button.
   * @param disabled
   *   Whether the button should be disabled.
   * @return
   *   The component.
   */
  def buttonViewColumn[A](
    button:       Button,
    onClick:      A => Callback,
    wrapperClass: Css = Css.Empty,
    disabled:     Boolean = false
  ) =
    ScalaComponent
      .builder[A]
      .render_P { rowData =>
        <.div(
          wrapperClass,
          button.addModifiers(Seq(^.onClick ==> (_ => onClick(rowData)), ^.disabled := disabled))
        )
      }
      .build
      .cmapCtorProps[CellProps[_, A, _]](_.value)
      .toJsComponent
      .raw

  private def newId: NonEmptyString = NonEmptyString
    .from("Id" + java.util.UUID.randomUUID().toString().replace("-", ""))
    .getOrElse("idddd")
}
