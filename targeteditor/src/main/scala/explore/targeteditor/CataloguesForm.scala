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

final case class CataloguesForm(
  fov:              Boolean
)(implicit val ctx: AppContextIO)
    extends ReactProps[CataloguesForm](CataloguesForm.component)

object CataloguesForm {
  type Props = CataloguesForm

  implicit val propsReuse: Reusability[CataloguesForm] = Reusability.derive

  val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P(p =>
        Form(size = Mini)(
          FormDropdown(
            label = "Catalogues",
            value = 0,
            selection = true,
            options = List(DropdownItem(value = 0, text = "DSS2 Gemini"),
                           DropdownItem(value = 1, text = "Spitzer")
            )
          ),
          FormCheckbox(label = "FOV", checked = p.fov),
          FormCheckbox(label = "Guiding"),
          FormCheckbox(label = "Catalog"),
          FormCheckbox(label = "Offsets")
        )
      )
      .configure(Reusability.shouldComponentUpdate)
      .build

}
