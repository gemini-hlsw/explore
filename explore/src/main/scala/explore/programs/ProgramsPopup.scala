// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.ProgramQueries
import explore.common.ProgramQueries.ProgramInfo
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ModelUndoStacks
import explore.model.enum.AppTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.ui.reusability._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scalajs.js

final case class ProgramsPopup(
  currentProgramId: Option[Program.Id],
  undoStacks:       ReuseView[ModelUndoStacks[IO]],
  trigger:          Option[Callback ==> VdomNode] = none
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ProgramsPopup](ProgramsPopup.component)

object ProgramsPopup {
  type Props = ProgramsPopup

  implicit val reuseProps: Reusability[Props] = Reusability.derive

  protected def addProgram(programs: ReuseView[List[ProgramInfo]], adding: ReuseView[Boolean])(
    implicit ctx:                    AppContextIO
  ): IO[Unit] =
    adding.async.set(true) >>
      ProgramQueries
        .createProgram[IO](none)
        .flatMap {
          _.foldMap(pi => programs.async.mod(_ :+ pi))
        }
        .guarantee(adding.async.set(false))

  protected def selectProgram(
    isRequired: Boolean,
    isOpen:     ReuseView[Boolean],
    undoStacks: ReuseView[ModelUndoStacks[IO]],
    programId:  Program.Id
  )(implicit
    ctx:        AppContextIO
  ): Callback =
    isOpen.set(false) >>
      undoStacks.set(ModelUndoStacks[IO]()) >>
      (if (isRequired) ctx.replacePage(AppTab.Overview, programId, none, none)
       else ctx.pushPage(AppTab.Overview, programId, none, none))

  protected def onNewData(
    isOpen:     ReuseView[Boolean],
    isRequired: Boolean,
    programs:   List[ProgramInfo]
  )(implicit
    ctx:        AppContextIO
  ): IO[Unit] =
    (isRequired, programs) match {
      case (false, _)          => IO.unit
      case (true, head :: Nil) => ctx.replacePage(AppTab.Overview, head.id, none, none).to[IO]
      case _                   => isOpen.set(true).to[IO]
    }

  protected def showModal(
    props:       Props,
    isOpen:      ReuseView[Boolean],
    adding:      ReuseView[Boolean],
    showDeleted: ReuseView[Boolean],
    isRequired:  Boolean,
    programs:    ReuseView[List[ProgramInfo]]
  ): VdomNode = {
    implicit val ctx = props.ctx

    val actions =
      if (isRequired) List.empty
      else
        List(
          Button(size = Small, icon = true)(Icons.Close, "Cancel")(^.tpe := "button",
                                                                   ^.key := "input-cancel"
          )
        )

    React.Fragment(
      Modal(
        clazz = ExploreStyles.ProgramsPopup,
        actions = actions,
        centered = false,
        open = isOpen.get,
        closeOnDimmerClick = !isRequired,
        closeOnEscape = !isRequired,
        closeIcon =
          if (isRequired) js.undefined
          else Icons.Close.clazz(ExploreStyles.ModalCloseButton),
        dimmer = Dimmer.Blurring,
        size = ModalSize.Small,
        onClose = isOpen.set(false),
        header = ModalHeader(
          "Programs"
        ),
        content = ModalContent(
          ProgramTable(
            props.currentProgramId,
            programs,
            showDeleted.get,
            selectProgram = Reuse.currying(isRequired, isOpen, props.undoStacks).in(selectProgram _)
          ),
          <.div(
            Button(
              clazz = ExploreStyles.ProgramAdd,
              compact = true,
              icon = Icons.New,
              content = "Program",
              disabled = adding.get,
              loading = adding.get,
              onClick = addProgram(programs, adding).runAsync
            ),
            Checkbox(label = "Show deleted",
                     checked = showDeleted.get,
                     onChange = showDeleted.set _
            )
          )
        )
      )
    )
  }

  protected val component = ScalaFnComponent
    .withHooks[Props]
    .useStateViewWithReuse(false) // isOpen
    .useStateViewWithReuse(false) // Adding new program
    .useStateViewWithReuse(false) // Show deleted
    .useMemoBy((props, _, _, _) => props.trigger)((_, _, _, _) =>
      _.isEmpty                   // if no trigger, program id was missing or invalid
    )
    .useMemoBy((props, isOpen, adding, showDeleted, isRequired) =>
      (props, isOpen, adding, showDeleted, isRequired)
    ) { (props, isOpen, adding, showDeleted, isRequired) => _ =>
      implicit val ctx = props.ctx
      if (isOpen.get || isRequired)
        ProgramQueries
          .ProgramsLiveQuery(Reuse(showModal _)(props, isOpen, adding, showDeleted, isRequired),
                             true,
                             Reuse(onNewData _)(isOpen, isRequired)
          )
          .some
      else none
    }
    .renderWithReuse { (props, isOpen, _, _, _, popup) =>
      React.Fragment(props.trigger.map(_(isOpen.set(true))), popup.value)
    }
}
