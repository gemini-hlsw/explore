// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import explore.Icons
import explore.common.ProgramQueries
import explore.common.ProgramQueries._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ModelUndoStacks
import explore.model.enums.AppTab
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types._
import queries.common.ProgramQueriesGQL._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scalajs.js.JSConverters._

final case class ProgramsPopup(
  currentProgramId: Option[Program.Id],
  undoStacks:       View[ModelUndoStacks[IO]],
  onClose:          Option[Callback] = none
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ProgramsPopup](ProgramsPopup.component)

object ProgramsPopup {
  type Props = ProgramsPopup

  protected def addProgram(programs: View[List[ProgramInfo]], adding: View[Boolean])(implicit
    ctx:                             AppContextIO
  ): IO[Unit] =
    adding.async.set(true) >>
      ProgramQueries
        .createProgram[IO](none)
        .flatMap(pi => programs.async.mod(_ :+ pi))
        .guarantee(adding.async.set(false))

  protected def selectProgram(
    onClose:      Option[Callback],
    undoStacks:   View[ModelUndoStacks[IO]]
  )(
    programId:    Program.Id
  )(implicit ctx: AppContextIO): Callback =
    onClose.orEmpty >>
      undoStacks.set(ModelUndoStacks[IO]()) >>
      (if (onClose.isEmpty) ctx.replacePage(AppTab.Overview, programId, none, none)
       else ctx.pushPage(AppTab.Overview, programId, none, none))

  protected def onNewData(
    isRequired:   Boolean,
    programs:     List[ProgramInfo]
  )(implicit ctx: AppContextIO): IO[Unit] =
    (isRequired, programs) match {
      case (true, head :: Nil) => ctx.replacePage(AppTab.Overview, head.id, none, none).to[IO]
      case _                   => IO.unit
    }

  protected def showModal(
    props:       Props,
    adding:      View[Boolean],
    showDeleted: View[Boolean],
    programs:    View[List[ProgramInfo]]
  ): VdomNode = {
    implicit val ctx = props.ctx

    val actions =
      if (props.onClose.isEmpty) List.empty
      else
        List(
          Button(size = Small, icon = true)(Icons.Close, "Cancel")(
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
      header = ModalHeader("Programs"),
      content = ModalContent(
        ProgramTable(
          props.currentProgramId,
          programs,
          showDeleted.get,
          selectProgram = selectProgram(props.onClose, props.undoStacks)
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
          Checkbox(label = "Show deleted", checked = showDeleted.get, onChange = showDeleted.set)
        )
      )
    )
  }

  protected val component = ScalaFnComponent
    .withHooks[Props]
    .useStateView(false) // Adding new program
    .useStateView(false) // Show deleted
    .useStreamResourceViewBy((_, _, showDeleted) => showDeleted.get) {
      (props, _, _) => showDeleted =>
        implicit val ctx = props.ctx

        val whereExistence =
          if (showDeleted) WhereEqExistence(IN = List(Existence.Deleted, Existence.Present).assign)
          else WhereEqExistence(EQ = Existence.Present.assign)

        ProgramsQuery
          .query(WhereProgram(existence = whereExistence.assign))
          .map(ProgramsQuery.Data.asProgramInfoList)
          .flatTap(programs => onNewData(props.onClose.isEmpty, programs))
          .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO]())
    }
    .render((props, adding, showDeleted, programs) =>
      potRender(p => showModal(props, adding, showDeleted, p))(programs)
    )
}
