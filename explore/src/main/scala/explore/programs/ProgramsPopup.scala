// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ModelUndoStacks
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.*
import react.semanticui.shorthand.*
import react.semanticui.sizes.*

import scalajs.js.JSConverters.*

case class ProgramsPopup(
  currentProgramId: Option[Program.Id],
  undoStacks:       View[ModelUndoStacks[IO]],
  onClose:          Option[Callback] = none
) extends ReactFnProps(ProgramsPopup.component)

object ProgramsPopup {
  private type Props = ProgramsPopup

  private def selectProgram(
    onClose:    Option[Callback],
    undoStacks: View[ModelUndoStacks[IO]],
    ctx:        AppContext[IO]
  )(programId:  Program.Id): Callback =
    onClose.orEmpty >>
      undoStacks.set(ModelUndoStacks[IO]()) >>
      (if (onClose.isEmpty) ctx.replacePage(AppTab.Overview, programId, Focused.None)
       else ctx.pushPage(AppTab.Overview, programId, Focused.None))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .render { (props, ctx) =>
      val actions =
        if (props.onClose.isEmpty) List.empty
        else
          List(
            Button(size = Small, icon = true, negative = true)(Icons.Close, "Cancel")(
              ^.tpe := "button",
              ^.key := "input-cancel"
            )
          )

      Modal(
        clazz = ExploreStyles.ProgramsPopup,
        actions = actions,
        centered = false,
        open = true,
        closeOnDimmerClick = props.onClose.isDefined,
        closeOnEscape = props.onClose.isDefined,
        closeIcon = props.onClose
          .map(_ => Icons.Close.clazz(ExploreStyles.ModalCloseButton): VdomNode)
          .orUndefined,
        dimmer = Dimmer.Blurring,
        size = ModalSize.Small,
        onClose = props.onClose.orUndefined,
        header = ModalHeader(content = "Programs"),
        content = ModalContent(
          ProgramTable(
            props.currentProgramId,
            selectProgram = selectProgram(props.onClose, props.undoStacks, ctx),
            props.onClose.isEmpty
          )
        )
      )
    }
}
