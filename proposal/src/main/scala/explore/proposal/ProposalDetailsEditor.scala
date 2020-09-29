// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.ui.GPPStyles
import explore.components.FormStaticData
import explore.components.Tile
import explore.model._
import explore.model.display._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms._
import react.common.implicits._
import react.common.ReactProps
import react.semanticui.collections.form._
import react.semanticui.addons.textarea.TextArea

final case class ProposalDetailsEditor(proposalDetails: View[ProposalDetails])
    extends ReactProps[ProposalDetailsEditor](ProposalDetailsEditor.component)

object ProposalDetailsEditor {
  type Props = ProposalDetailsEditor

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        val details = props.proposalDetails
        <.div(
          <.div(
            ^.key := "details",
            GPPStyles.ProposalTile,
            Tile("Details", movable = false)(
              Form(
                GPPStyles.TwoColumnGrid,
                FormInputEV(
                  id = "title",
                  name = "title",
                  className = "inverse",
                  value = details.zoom(ProposalDetails.title),
                  label = "Title"
                ).withMods(^.autoFocus := true),
                EnumViewSelect(details.zoom(ProposalDetails.category).asOpt, label = "Category"),
                <.div(
                  GPPStyles.CellWrapper,
                  FormInput(
                    value = details.get.pi.displayName,
                    label = "Principle Investigator",
                    clazz = GPPStyles.StaticData |+| GPPStyles.GrowOne
                  )(
                    ^.readOnly := true
                  ),
                  FormButton(icon = Icons.Edit,
                             label = "Edit Principle Investigator",
                             clazz = GPPStyles.HideLabel
                  )
                ),
                EnumViewMultipleSelect(
                  details.zoom(ProposalDetails.keywords),
                  label = "Keywords",
                  search = true
                ),
                <.div(
                  GPPStyles.CellWrapper,
                  FormStaticData(value = "7.50h", id = "requested", label = "Requested Time"),
                  FormStaticData(value = "<partner splits coming soon>",
                                 id = "splits",
                                 label = "Splits",
                                 hideLabel = true
                  ),
                  FormButton(icon = Icons.Edit,
                             label = "Edit requested time split",
                             clazz = GPPStyles.HideLabel |+| GPPStyles.ToEnd
                  )
                ),
                EnumViewSelect(details.zoom(ProposalDetails.toOActivation).asOpt,
                               label = "ToO Activation"
                ),
                EnumViewSelect(details.zoom(ProposalDetails.proposalClass).asOpt, label = "Class"),
                FormStaticData(value = "<Determined by observations>",
                               label = "Band 3",
                               id = "band3"
                )
              )
            )
          ),
          <.div(
            ^.key := "abstract",
            GPPStyles.ProposalTile,
            Tile("Abstract", movable = false)(
              Form(
                TextArea(
                  rows = 10,
                  value = details.zoom(ProposalDetails.abstrakt).get,
                  onChangeE = (_: Form.ReactFormEvent, tap: TextArea.TextAreaProps) => {
                    details
                      .zoom(ProposalDetails.abstrakt)
                      .set(tap.value.asInstanceOf[String])
                      .runInCB
                  }
                )
              )
            )
          ),
          <.div(
            ^.key := "preview",
            GPPStyles.ProposalTile,
            Tile("Preview", movable = false)(
              <.span("Placeholder for PDF preview.")
            )
          )
        )
      }
      .build

}
