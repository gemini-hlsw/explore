// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import cats.Show
import cats.implicits._
import crystal.react.AppRoot
import crystal.react.implicits._
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.components.undo.UndoRegion
import explore.conditions.ConditionsQueries._
import explore.implicits._
import explore.model.Conditions
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

final case class ConditionsPanel(
  observationId: Observation.Id,
  conditions:    View[Conditions]
) extends ReactProps[ConditionsPanel](ConditionsPanel.component)

object ConditionsPanel {
  type Props = ConditionsPanel

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[ConditionsPanel]
      .render { $ =>
        val conditions = $.props.conditions

        UndoRegion[Conditions] { undoCtx =>
          val undoViewZoom =
            UndoViewZoom($.props.observationId, conditions, undoCtx.setter)

          AppCtx.withCtx { implicit appCtx =>
            def selectEnum[A: Enumerated: Show](
              label:  String,
              lens:   Lens[Conditions, A],
              fields: A => Mutation.Fields
            ) =
              EnumViewSelect(undoViewZoom(lens, fields).asOpt, label = label)

            <.div(
              Form(
                FormGroup(widths = Two)(
                  selectEnum("Image Quality", Conditions.iq, iqFields),
                  selectEnum("Cloud Cover", Conditions.cc, ccFields)
                ),
                FormGroup(widths = Two)(
                  selectEnum("Water Vapor", Conditions.wv, wvFields),
                  selectEnum("Sky Background", Conditions.sb, sbFields)
                )
              ),
              UndoButtons(conditions.get, undoCtx)
            )
          }
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
