// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.form._
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._
import explore.model.TargetVisualOptions
import explore.model.reusability._
import monocle.macros.Lenses
import monocle.Lens
import explore.model.enum.Display
import react.semanticui.addons.select.Select
import cats.data.NonEmptyList
import gsp.math.Angle
import scala.collection.SortedMap
import react.semanticui.collections.form.FormDropdown.FormDropdownProps

final case class CataloguesForm(
  options:          TargetVisualOptions,
  updateOptions:    TargetVisualOptions => Callback
)(implicit val ctx: AppContextIO)
    extends ReactProps[CataloguesForm](CataloguesForm.component)

object CataloguesForm {
  type Props = CataloguesForm

  @Lenses
  final case class State(options: TargetVisualOptions)

  object State {
    def displayLens(l: Lens[TargetVisualOptions, Display]): Lens[State, Boolean] =
      State.options ^|-> l ^<-> Display.boolReverseIso

    val posAngle: Lens[State, Angle] =
      State.options ^|-> TargetVisualOptions.posAngle

    val fov: Lens[State, Boolean] =
      displayLens(TargetVisualOptions.fov)

    val guiding: Lens[State, Boolean] =
      displayLens(TargetVisualOptions.guiding)

    val offsets: Lens[State, Boolean] =
      displayLens(TargetVisualOptions.offsets)

    val probe: Lens[State, Boolean] =
      displayLens(TargetVisualOptions.probe)
  }

  implicit val propsReuse: Reusability[CataloguesForm] = Reusability.by(_.options)
  implicit val stateReuse: Reusability[State]          = Reusability.derive

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

  val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(p => State(p.options))
      .render($ =>
        Form(size = Mini)(
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
            checked = $.state.options.fov.visible,
            onChange = (b: Boolean) => {
              val ns = State.fov.set(b)($.state)
              $.setState(ns, $.props.updateOptions(ns.options))
            }
          ),
          FormCheckbox(
            label = "Patrol field",
            checked = $.state.options.guiding.visible,
            onChange = (b: Boolean) => {
              val ns = State.guiding.set(b)($.state)
              $.setState(ns, $.props.updateOptions(ns.options))
            }
          ),
          FormCheckbox(
            label = "Probe",
            checked = $.state.options.probe.visible,
            onChange = (b: Boolean) => {
              val ns = State.probe.set(b)($.state)
              $.setState(ns, $.props.updateOptions(ns.options))
            }
          ),
          FormCheckbox(
            label = "Offsets",
            checked = $.state.options.offsets.visible,
            onChange = (b: Boolean) => {
              val ns = State.offsets.set(b)($.state)
              $.setState(ns, $.props.updateOptions(ns.options))
            }
          ),
          FormSelect(
            label = "Position Angle",
            options = angleItems.toList,
            value = Angle.degrees.get($.state.options.posAngle),
            onChange = (p: FormDropdownProps) => {
              angleItemsMap
                .collectFirst {
                  case (a, i) if i.value == p.value => a
                }
                .map { a =>
                  val ns = State.posAngle.set(a)($.state)
                  $.setState(ns, $.props.updateOptions(ns.options))
                }
                .getOrEmpty

            }
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
      .build

}
