// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import _root_.react.common.ReactFnProps
import _root_.react.primereact.Dialog
import _root_.react.primereact.DialogPosition
import _root_.react.primereact.Message
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.View
import crystal.react.ViewOpt
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ModelUndoStacks
import explore.model.ProgramInfoList
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

import scalajs.js.JSConverters.*

case class ProgramsPopup(
  currentProgramId: Option[Program.Id],
  programInfos:     ViewOpt[ProgramInfoList],
  undoStacks:       View[ModelUndoStacks[IO]],
  onClose:          Option[Callback] = none,
  message:          Option[String] = none
) extends ReactFnProps(ProgramsPopup.component)

object ProgramsPopup {
  private type Props = ProgramsPopup

  private object IsOpen extends NewType[Boolean]
  private type IsOpen = IsOpen.Type

  private def selectProgram(
    onClose:    Option[Callback],
    undoStacks: View[ModelUndoStacks[IO]],
    ctx:        AppContext[IO]
  )(programId: Program.Id): Callback =
    onClose.orEmpty >>
      undoStacks.set(ModelUndoStacks[IO]()) >>
      (if (onClose.isEmpty) ctx.replacePage(AppTab.Overview, programId, Focused.None)
       else ctx.pushPage(AppTab.Overview, programId, Focused.None))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(IsOpen(true))
    .render { (props, ctx, isOpen) =>
      val onHide = props.onClose.map(oc => isOpen.set(IsOpen(false)) >> oc)

      Dialog(
        visible = isOpen.get.value,
        onHide = onHide.orEmpty,
        position = DialogPosition.Top,
        closeOnEscape = props.onClose.isDefined,
        closable = props.onClose.isDefined,
        dismissableMask = props.onClose.isDefined,
        resizable = false,
        clazz = ExploreStyles.Dialog.Small |+| ExploreStyles.ProgramsPopup,
        header = "Programs"
      )(
        <.div(
          props.programInfos
            .mapValue(pis =>
              ProgramTable(
                props.currentProgramId,
                pis,
                selectProgram = selectProgram(props.onClose, props.undoStacks, ctx),
                props.onClose.isEmpty,
                onHide
              )
            )
            .toPot
            .renderPot(identity)
        ),
        props.message.map(msg =>
          Message(text = msg, severity = Message.Severity.Warning, icon = Icons.ExclamationTriangle)
        )
      )
    }
}
