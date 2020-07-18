// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Show
import crystal.react.implicits._
import explore.AppCtx
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.constraints.ConstraintsQueries._
import explore.implicits._
import explore.model.Constraints
import explore.model.reusability._
import explore.model.show._
import gem.util.Enumerated
import gpp.ui.forms.EnumViewSelect
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import react.common._
import react.semanticui.collections.grid._
import react.semanticui.collections.form.Form
import react.semanticui.collections.form.FormGroup
import react.semanticui.widths._

final case class ConstraintsPanel(
  id:          Constraints.Id,
  constraints: View[Constraints]
) extends ReactProps[ConstraintsPanel](ConstraintsPanel.component)

object ConstraintsPanel {
  type Props = ConstraintsPanel

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[ConstraintsPanel]
      .render { $ =>
        val constraints = $.props.constraints

        UndoRegion[Constraints] { undoCtx =>
          val undoViewZoom =
            UndoViewZoom($.props.id, constraints, undoCtx.setter)

          AppCtx.withCtx { implicit appCtx =>
            def selectEnum[A: Enumerated: Show](
              label:  String,
              lens:   Lens[Constraints, A],
              fields: A => Mutation.Fields
            ) =
              EnumViewSelect(undoViewZoom(lens, fields).asOpt, label = label)

            Grid(columns = One, stretched = true, padded = GridPadded.Horizontally)(
              GridColumn(
                GridRow(
                  Form(
                    FormGroup(widths = Two)(
                      constraints.get.name
                    ),
                    FormGroup(widths = Two)(
                      selectEnum("Image Quality", Constraints.iq, iqFields),
                      selectEnum("Cloud Cover", Constraints.cc, ccFields)
                    ),
                    FormGroup(widths = Two)(
                      selectEnum("Water Vapor", Constraints.wv, wvFields),
                      selectEnum("Sky Background", Constraints.sb, sbFields)
                    )
                  )
                ),
                GridRow(
                  UndoButtons(constraints.get, undoCtx)
                )
              )
            )
          }
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
