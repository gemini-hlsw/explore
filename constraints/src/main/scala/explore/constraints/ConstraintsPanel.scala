// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Show
import cats.implicits._
import crystal.react.AppRoot
import crystal.react.implicits._
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.components.undo.UndoRegion
import explore.constraints.ConstraintsQueries._
import explore.implicits._
import explore.model.Constraints
import explore.model.enum.CloudCover
import explore.model.enum.ImageQuality
import explore.model.enum.SkyBackground
import explore.model.enum.WaterVapor
import explore.model.reusability._
import explore.model.show._
import gem.Observation
import gem.util.Enumerated
import gpp.ui.forms.EnumViewSelect
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.collections.form.FormGroup
import react.semanticui.elements.button.Button
import react.semanticui.widths._
import explore.components.undo.UndoButtons
import react.semanticui.collections.form.FormField
import react.semanticui.elements.label.Label

final case class ConstraintsPanel(
  observationId: Observation.Id,
  Constraints:   View[Constraints]
) extends ReactProps[ConstraintsPanel](ConstraintsPanel.component)

object ConstraintsPanel {
  type Props = ConstraintsPanel

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[ConstraintsPanel]
      .render { $ =>
        val constraints = $.props.Constraints

        UndoRegion[Constraints] { undoCtx =>
          val undoViewZoom =
            UndoViewZoom($.props.observationId, constraints, undoCtx.setter)

          AppCtx.withCtx { implicit appCtx =>
            def selectEnum[A: Enumerated: Show](
              label:  String,
              lens:   Lens[Constraints, A],
              fields: A => Mutation.Fields
            ) =
              EnumViewSelect(undoViewZoom(lens, fields).asOpt, label = label)

            <.div(
              Form(
                FormGroup(widths = Two)(
                  selectEnum("Image Quality", Constraints.iq, iqFields),
                  selectEnum("Cloud Cover", Constraints.cc, ccFields)
                ),
                FormGroup(widths = Two)(
                  selectEnum("Water Vapor", Constraints.wv, wvFields),
                  selectEnum("Sky Background", Constraints.sb, sbFields)
                )
              ),
              UndoButtons(constraints.get, undoCtx)
            )
          }
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
