// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Magnitude
import lucuma.ui.reusability._
import react.common.ReactProps
import react.semanticui.collections.grid.Grid
import react.semanticui.collections.grid.GridColumn
import react.semanticui.collections.grid.GridDivided
import react.semanticui.collections.grid.GridRow
import react.semanticui.elements.segment.Segment
import react.semanticui.widths._

final case class MagnitudeForm(magnitudes: List[Magnitude])
    extends ReactProps[MagnitudeForm](MagnitudeForm.component)

object MagnitudeForm {
  type Props = MagnitudeForm

  implicit val propsReuse = Reusability.derive[Props]

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        React.Fragment(
          <.label("Magnitudes"),
          Segment(
            Grid(columns = Three, divided = GridDivided.Divided)(
              props.magnitudes.toTagMod(magnitude =>
                GridRow(
                  GridColumn(<.span(magnitude.value.toDoubleValue.toString)),
                  GridColumn(<.span(magnitude.band.toString)),
                  GridColumn(<.span(magnitude.system.toString))
                )(^.paddingTop := "0.5rem", ^.paddingBottom := "0.5rem")
              )
            )
          )(^.maxWidth := "250px")
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
