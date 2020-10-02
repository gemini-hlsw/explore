// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore._
import explore.components.FormStaticData
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model._
import explore.model.display._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms._
import react.common.ReactProps
import react.common.implicits._
import react.semanticui.addons.textarea.TextArea
import react.semanticui.collections.form._

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
            ExploreStyles.ProposalTile,
            Tile("Details", movable = false)(
              Form(
                ExploreStyles.TwoColumnGrid,
                FormInputEV(
                  id = "title",
                  className = "inverse",
                  value = details.zoom(ProposalDetails.title),
                  label = "Title"
                ).withMods(^.autoFocus := true),
                EnumViewSelect(id = "category",
                               value = details.zoom(ProposalDetails.category).asOpt,
                               label = "Category"
                ),
                <.div(
                  ExploreStyles.FlexContainer,
                  FormInput(
                    value = details.get.pi.displayName,
                    label = "Principle Investigator",
                    clazz = ExploreStyles.StaticData |+| ExploreStyles.Grow(1)
                  )(
                    ^.readOnly := true
                  ),
                  FormButton(icon = Icons.Edit,
                             label = "Edit Principle Investigator",
                             clazz = ExploreStyles.HideLabel
                  )
                ),
                EnumViewMultipleSelect(
                  id = "keywords",
                  value = details.zoom(ProposalDetails.keywords),
                  label = "Keywords",
                  search = true
                ),
                <.div(
                  ExploreStyles.FlexContainer,
                  FormStaticData(value = "7.50h", id = "requested", label = "Requested Time"),
                  FormStaticData(value = "<partner splits coming soon>",
                                 id = "splits",
                                 label = "Splits",
                                 hideLabel = true
                  ),
                  FormButton(icon = Icons.Edit,
                             label = "Edit requested time split",
                             clazz = ExploreStyles.HideLabel |+| ExploreStyles.ToEnd
                  )
                ),
                EnumViewSelect(id = "too-activation",
                               value = details.zoom(ProposalDetails.toOActivation).asOpt,
                               label = "ToO Activation"
                ),
                EnumViewSelect(id = "proposal-class",
                               value = details.zoom(ProposalDetails.proposalClass).asOpt,
                               label = "Class"
                ),
                FormStaticData(value = "<Determined by observations>",
                               label = "Band 3",
                               id = "band3"
                )
              )
            )
          ),
          <.div(
            ^.key := "abstract",
            ExploreStyles.ProposalTile,
            Tile("Abstract", movable = false)(
              Form(
                TextArea(
                  rows = 10,
                  value = details.zoom(ProposalDetails.abstrakt).get,
                  onChangeE = (_: Form.ReactChangeEvent, tap: TextArea.TextAreaProps) => {
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
            ExploreStyles.ProposalTile,
            Tile("Preview", movable = false)(
              <.span("Placeholder for PDF preview.")
            )
          )
        )
      }
      .build

}
