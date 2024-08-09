// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.View
import crystal.react.ViewOpt
import crystal.react.hooks.*
import explore.Icons
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ProgramInfo
import explore.model.ProgramInfoList
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.syntax.ui.*
import explore.undo.UndoStacks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.Message
import lucuma.react.table.HTMLTableVirtualizer
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger

case class ProgramsPopup(
  currentProgramId: Option[Program.Id],
  programInfos:     ViewOpt[ProgramInfoList],
  undoStacks:       View[UndoStacks[IO, ProgramSummaries]],
  onClose:          Option[Callback] = none,
  onLogout:         Option[IO[Unit]] = none,
  message:          Option[String] = none
) extends ReactFnProps(ProgramsPopup.component)

object ProgramsPopup:
  private type Props = ProgramsPopup

  private object IsOpen extends NewType[Boolean]

  private object IsAdding extends NewType[Boolean]
  private type IsAdding = IsAdding.Type

  private object ShowDeleted extends NewType[Boolean]

  private def selectProgram(
    onClose:    Option[Callback],
    undoStacks: View[UndoStacks[IO, ProgramSummaries]],
    ctx:        AppContext[IO]
  )(programId: Program.Id): Callback =
    onClose.orEmpty >>
      undoStacks.set(UndoStacks.empty[IO, ProgramSummaries]) >>
      (if (onClose.isEmpty) ctx.replacePage(AppTab.Overview, programId, Focused.None)
       else ctx.pushPage(AppTab.Overview, programId, Focused.None))

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
      .setAlign(rawVirtual.mod.ScrollAlignment.start) // force to go as far as possible

  private def addProgram(
    programs: View[ProgramInfoList],
    adding:   View[IsAdding]
  )(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): IO[Unit] =
    ProgramQueries
      .createProgram[IO](none)
      .flatMap(pi => programs.async.mod(_.updated(pi.id, pi)))
      .switching(adding.async, IsAdding(_))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(IsOpen(true))
    .useStateView(IsAdding(false))    // Adding new program
    .useStateView(ShowDeleted(false)) // Show deleted
    .useRef(none[HTMLTableVirtualizer])
    .render: (props, ctx, isOpen, isAdding, showDeleted, virtualizerRef) =>
      import ctx.given

      val onHide = props.onClose.map(oc => isOpen.set(IsOpen(false)) >> oc)

      val closeButton =
        props.onClose.fold(none)(cb =>
          Button(
            label = "Cancel",
            icon = Icons.Close,
            severity = Button.Severity.Danger,
            onClick = cb
          ).small.compact.some
        )

      val logoutButton =
        props.onLogout.fold(none)(io =>
          Button(
            label = "Logout",
            icon = Icons.Logout,
            severity = Button.Severity.Danger,
            onClick = (ctx.sso.logout >> io).runAsync
          ).small.compact.some
        )

      val programInfosViewOpt: Option[View[ProgramInfoList]] =
        props.programInfos.toOptionView

      val programInfoViewListOpt: Option[List[View[ProgramInfo]]] =
        programInfosViewOpt.map:
          _.toListOfViews
            .map(_._2)
            .filter(vpi => showDeleted.get.value || !vpi.get.deleted)
            .sortBy(_.get.id)

      // When "isAdding" is switched off, it is safe to scroll to the bottom since the new program has been added.
      val isAddingWithScroll: View[IsAdding] =
        isAdding.withOnMod: newValue =>
          if (!newValue.value)
            virtualizerRef.get
              .map:
                _.foreach: virtualizer =>
                  virtualizer.scrollToIndex(virtualizer.getTotalSize(), ScrollOptions)
          else Callback.empty

      Dialog(
        visible = isOpen.get.value,
        onHide = onHide.orEmpty,
        position = DialogPosition.Top,
        closeOnEscape = props.onClose.isDefined,
        closable = props.onClose.isDefined,
        dismissableMask = props.onClose.isDefined,
        resizable = false,
        clazz = LucumaPrimeStyles.Dialog.Small |+| ExploreStyles.ProgramsPopup,
        header = "Programs",
        footer = programInfosViewOpt.map: pis =>
          React.Fragment(
            Button(
              label = "Program",
              icon = Icons.New,
              severity = Button.Severity.Success,
              disabled = isAdding.get.value,
              loading = isAdding.get.value,
              onClick = addProgram(pis, isAddingWithScroll).runAsync
            ).small.compact,
            CheckboxView(
              id = "show-deleted".refined,
              value = showDeleted.zoom(ShowDeleted.value.asLens),
              label = "Show deleted"
            ),
            closeButton,
            logoutButton
          )
      )(
        programInfoViewListOpt.toPot
          .renderPot: pis =>
            ProgramTable(
              props.currentProgramId,
              pis,
              selectProgram = selectProgram(props.onClose, props.undoStacks, ctx),
              props.onClose.isEmpty,
              onHide,
              props.onLogout,
              virtualizerRef
            ),
        props.message.map(msg =>
          Message(text = msg, severity = Message.Severity.Warning, icon = Icons.ExclamationTriangle)
        )
      )
