// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import explore._
import explore.implicits._
import explore.model.SiderealTarget
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.form._
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._

final case class CataloguesForm(
  target:           SiderealTarget
)(implicit val ctx: AppContextIO)
    extends ReactProps[CataloguesForm](CataloguesForm.component)

object CataloguesForm {
  type Props = CataloguesForm

  val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render(_ =>
        Form(size = Mini)(
          FormDropdown(
            label = "Catalogues",
            value = 0,
            selection = true,
            options = List(DropdownItem(value = 0, text = "DSS2 Gemini"),
                           DropdownItem(value = 1, text = "Non-sidereal")
            )
          ),
          FormCheckbox(label = "FOV"),
          FormCheckbox(label = "Guiding"),
          FormCheckbox(label = "Catalog"),
          FormCheckbox(label = "Offsets")
        )
      )
      .build

}
