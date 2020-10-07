// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.collection.SortedMap

import cats.data.NonEmptyList
import crystal.react.implicits._
import explore.View
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.enum.Display
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle
import react.common._
import react.semanticui.addons.select.Select
import react.semanticui.collections.form.FormDropdown.FormDropdownProps
import react.semanticui.collections.form._
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._

final case class CataloguesForm(
  options:          View[TargetVisualOptions]
)(implicit val ctx: AppContextIO)
    extends ReactProps[CataloguesForm](CataloguesForm.component)

object CataloguesForm {
  type Props = CataloguesForm
  implicit val propsReuse: Reusability[CataloguesForm] = Reusability.derive

  // List of allowed angles, this would come from the model
  val angles: NonEmptyList[Angle] = NonEmptyList.of(Angle.Angle0,
                                                    Angle.Angle90,
                                                    GmosGeometry.posAngle,
                                                    Angle.Angle180,
                                                    Angle.Angle270
  )

  val angleItemsMap: SortedMap[Angle, Select.SelectItem] = SortedMap.from(angles.map { a =>
    val value = Angle.degrees.get(a)
    a -> new Select.SelectItem(value = value, text = value.toString)
  }.toList)(Angle.AngleOrder.toOrdering)

  val angleItems = angleItemsMap.values

  val fovL     = TargetVisualOptions.fov ^<-> Display.boolReverseIso
  val guidingL = TargetVisualOptions.guiding ^<-> Display.boolReverseIso
  val probeL   = TargetVisualOptions.probe ^<-> Display.boolReverseIso
  val offsetsL = TargetVisualOptions.offsets ^<-> Display.boolReverseIso

  val component =
    ScalaComponent
      .builder[Props]
      .render { $ =>
        val optionsV = $.props.options
        val options  = optionsV.get
        Form(size = Small)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.CatalogueForm,
          FormDropdown(
            label = "Catalogues",
            value = 0,
            selection = true,
            options = List(DropdownItem(value = 0, text = "DSS2 Gemini"),
                           DropdownItem(value = 1, text = "Spitzer")
            )
          ),
          FormCheckbox(
            label = "CCD",
            checked = options.fov.visible,
            onChange = (b: Boolean) => optionsV.zoom(fovL).set(b).runInCB
          ),
          FormCheckbox(
            label = "Patrol field",
            checked = options.guiding.visible,
            onChange = (b: Boolean) => optionsV.zoom(guidingL).set(b).runInCB
          ),
          FormCheckbox(
            label = "Probe",
            checked = options.probe.visible,
            onChange = (b: Boolean) => optionsV.zoom(probeL).set(b).runInCB
          ),
          FormCheckbox(
            label = "Offsets",
            checked = options.offsets.visible,
            onChange = (b: Boolean) => optionsV.zoom(offsetsL).set(b).runInCB
          ),
          FormSelect(
            label = "Position Angle",
            options = angleItems.toList,
            value = Angle.degrees.get(options.posAngle),
            onChange = (p: FormDropdownProps) => {
              angleItemsMap
                .collectFirst {
                  case (a, i) if i.value == p.value => a
                }
                .map { a =>
                  optionsV.zoom(TargetVisualOptions.posAngle).set(a).runInCB
                }
                .getOrEmpty

            }
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
