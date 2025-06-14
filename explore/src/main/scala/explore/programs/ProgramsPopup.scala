// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Endo
import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ProgramInfo
import explore.model.ProgramInfoList
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.services.OdbProgramApi
import explore.syntax.ui.*
import explore.undo.UndoStacks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.NewBoolean
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.Message
import lucuma.react.table.HTMLTableVirtualizer
import lucuma.refined.*
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger

case class ProgramsPopup(
  currentProgramId: Option[Program.Id],
  userId:           User.Id,
  isStaff:          Boolean,
  programInfos:     ViewOpt[ProgramInfoList],
  undoStacks:       View[UndoStacks[IO, ProgramSummaries]],
  onClose:          Option[Callback] = none,
  message:          Option[String] = none
) extends ReactFnProps(ProgramsPopup.component)

object ProgramsPopup:
  private type Props = ProgramsPopup

  private object IsOpen extends NewBoolean

  private object IsAdding extends NewBoolean
  private type IsAdding = IsAdding.Type

  private object ShowDeleted extends NewBoolean

  private def selectProgram(
    onClose:    Option[Callback],
    undoStacks: View[UndoStacks[IO, ProgramSummaries]],
    ctx:        AppContext[IO]
  )(programId: Program.Id): Callback =
    onClose.orEmpty >>
      undoStacks.set(UndoStacks.empty[IO, ProgramSummaries]) >>
      (if (onClose.isEmpty) ctx.replacePage((AppTab.Overview, programId, Focused.None).some)
       else ctx.pushPage((AppTab.Overview, programId, Focused.None).some))

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
      .setAlign(rawVirtual.mod.ScrollAlignment.start) // force to go as far as possible

  private def addProgram(
    programsMod:     Endo[ProgramInfoList] => Callback,
    adding:          View[IsAdding],
    setNewProgramId: Program.Id => Callback
  )(using odbApi: OdbProgramApi[IO])(using Logger[IO]): IO[Unit] =
    odbApi
      .createProgram(none)
      .flatTap(pi => programsMod(_.updated(pi.id, pi)).to[IO])
      .flatMap(pi => setNewProgramId(pi.id).to[IO])
      .switching(adding.async, IsAdding(_))

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx            <- useContext(AppContext.ctx)
      isOpen         <- useStateView(IsOpen(true))
      isAdding       <- useStateView(IsAdding(false))    // Adding new program
      showDeleted    <- useStateView(ShowDeleted(false)) // Show deleted
      newProgramId   <- useStateView(none[Program.Id])   // Recently added program
      virtualizerRef <- useRef(none[HTMLTableVirtualizer])
    } yield
      import ctx.given

      val onHide: Callback =
        props.onClose.map(oc => isOpen.set(IsOpen(false)) >> oc).orEmpty >> newProgramId.set(none)

      val closeButton =
        props.onClose.fold(none): cb =>
          Button(
            label = "Cancel",
            icon = Icons.Close,
            severity = Button.Severity.Danger,
            onClick = cb
          ).small.compact.some

      val programInfosViewOpt: Option[View[ProgramInfoList]] =
        props.programInfos.toOptionView

      val doSelectProgram: Program.Id => Callback =
        selectProgram(props.onClose, props.undoStacks, ctx)

      val programInfoViewListOpt: Option[List[View[ProgramInfo]]] =
        programInfosViewOpt.map:
          _.toListOfViews
            .map(_._2.withOnMod(_ => newProgramId.set(none)))
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
        onHide = onHide,
        position = DialogPosition.Top,
        closeOnEscape = props.onClose.isDefined,
        closable = props.onClose.isDefined,
        modal = props.onClose.isDefined,
        dismissableMask = props.onClose.isDefined,
        resizable = true,
        clazz = LucumaPrimeStyles.Dialog.Large |+| ExploreStyles.ProgramsPopup,
        header = "Programs",
        footer = programInfosViewOpt.map: pis =>
          React.Fragment(
            Button(
              label = "Program",
              icon = Icons.New,
              severity = Button.Severity.Success,
              disabled = isAdding.get.value,
              loading = isAdding.get.value,
              onClick = addProgram(
                pis.mod,
                isAddingWithScroll,
                programId => newProgramId.set(programId.some)
              ).runAsync
            ).small.compact,
            CheckboxView(
              id = "show-deleted".refined,
              value = showDeleted.as(ShowDeleted.Value),
              label = "Show deleted"
            ),
            closeButton
          )
      )(
        programInfoViewListOpt.toPot
          .renderPot: pis =>
            ProgramTable(
              props.currentProgramId,
              props.userId,
              props.isStaff,
              pis,
              selectProgram = doSelectProgram,
              props.onClose.isEmpty,
              onHide.some,
              newProgramId.get,
              virtualizerRef
            ),
        props.message.map(msg =>
          Message(text = msg, severity = Message.Severity.Warning, icon = Icons.ExclamationTriangle)
        )
      )
