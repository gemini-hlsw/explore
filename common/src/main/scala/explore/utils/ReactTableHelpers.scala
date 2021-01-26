// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.{ Display, Enumerated }
import lucuma.ui.forms._
import lucuma.ui.optics._
import monocle.Lens
import react.semanticui.elements.button.Button
import reactST.reactTable.mod._

import scalajs.js

object ReactTableHelpers {

  /**
   * Create a AgGridColumn Builder for a column which edits the value in a FormInputEv.
   * Since this returns a Builder, additional column configuration methods can
   * be chained onto the return value.
   *
   * @param lens Lens to go between the Row value and the Cell value.
   * @param validFormat The ValidFormatInput for the editor.
   * @param changeAuditor The ChangeAuditor for the editor.
   * @param disabled Whether editing should be disabled or not.
   * @return An AgGridColumn Builder
   */
  def editableViewColumn[A, B](
    lens:          Lens[A, B],
    validFormat:   ValidFormatInput[B],
    changeAuditor: ChangeAuditor[B] = ChangeAuditor.accept[B],
    disabled:      Boolean = false,
    modifiers:     Seq[TagMod] = Seq.empty
  )(implicit eq:   Eq[B]) =
    ScalaComponent
      .builder[View[A]]
      .render_P { va =>
        FormInputEV(id = newId,
                    value = va.zoom(lens),
                    validFormat = validFormat,
                    changeAuditor = changeAuditor,
                    disabled = disabled,
                    modifiers = modifiers
        )
      }
      .build
      .cmapCtorProps[(CellProps[View[A], _]) with js.Object](_.cell.row.original)
      .toJsComponent
      .raw

  /**
   * Create a AgGridColumn Builder for a column which edits the value via a EnumViewSelect.
   * Obviously, the cell value must have an Enumerated instance.
   * Since this returns a Builder, additional column configuration methods can
   * be chained onto the return value.
   *
   * @param lens Lens to go between the Row value and the Cell value.
   * @param disabled Whether editing should be disabled or not.
   * @param excludeFn An optional function that determines a set of enums thst should be excluded from the dropdown.
   * @return An AgGridColumn Builder.
   */
  def editableEnumViewColumn[A, B](lens: Lens[A, B])(
    disabled:                            Boolean = false,
    excludeFn:                           Option[View[A] => Set[B]] = None,
    modifiers:                           Seq[TagMod] = Seq.empty
  )(implicit enum:                       Enumerated[B], display: Display[B]) =
    ScalaComponent
      .builder[View[A]]
      .render_P { va =>
        val excluded = excludeFn.fold(Set.empty[B])(_.apply(va))
        EnumViewSelect(id = newId,
                       value = va.zoom(lens),
                       exclude = excluded,
                       disabled = disabled,
                       modifiers = modifiers
        )
      }
      .build
      .cmapCtorProps[(CellProps[View[A], _]) with js.Object](_.cell.row.original)
      .toJsComponent
      .raw

  /**
   * Create a column containing a button.
   * The button is passed in to allow maximum flexibility, but the
   * onClick handler should not be specified for the button, since
   * it will be added here.
   *
   * The [A] type is the type of the entire data row.
   *
   * @param button The button to put in the cell.
   * @param onClick The onClick handler for the button.
   * @param disabled Whether the button should be disabled.
   * @return An AgGridColumn Builder.
   */
  def buttonViewColumn[A](
    button:   Button,
    onClick:  View[A] => Callback,
    disabled: Boolean = false
  ) =
    ScalaComponent
      .builder[View[A]]
      .render_P { rowData =>
        button.addModifiers(Seq(^.onClick ==> (_ => onClick(rowData)), ^.disabled := disabled))
      }
      .build
      .cmapCtorProps[(CellProps[View[A], _]) with js.Object](_.cell.row.original)
      .toJsComponent
      .raw

  private def newId: NonEmptyString = NonEmptyString
    .from("Id" + java.util.UUID.randomUUID().toString().replace("-", ""))
    .getOrElse("idddd")
}
