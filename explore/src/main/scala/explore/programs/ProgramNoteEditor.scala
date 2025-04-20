// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import eu.timepit.refined.cats.given
import explore.common.Aligner
import explore.components.MarkdownEditor
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ProgramNote
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import monocle.Iso
import org.scalajs.dom
import queries.common.ProgramQueriesGQL

final case class ProgramNoteEditor(
  note:              UndoSetter[ProgramNote],
  userIsReadonlyCoi: Boolean,
  userIsStaff:       Boolean
) extends ReactFnProps(ProgramNoteEditor)

object ProgramNoteEditor
    extends ReactFnComponent[ProgramNoteEditor](props =>
      for {
        ctx      <- useContext(AppContext.ctx)
        inputRef <- useRefToVdom[dom.HTMLInputElement]
        _        <- useEffectWithDeps((props.note.get.id, inputRef)): (_, ref) =>
                      ref.get
                        .flatMap(_.foldMap(i => Callback(i.select()).delayMs(50).toCallback))
                        .when_(props.note.get.title === ProgramNote.NewProgramNoteTitle)
      } yield
        import ctx.given

        val aligner: Aligner[ProgramNote, ProgramNotePropertiesInput] = Aligner(
          props.note,
          UpdateProgramNotesInput(
            WHERE = props.note.get.id.toWhereProgramNote.assign,
            SET = ProgramNotePropertiesInput()
          ),
          ProgramQueriesGQL.UpdateProgramNotesMutation[IO].execute(_).void
        ).zoom(Iso.id[ProgramNote].asLens, UpdateProgramNotesInput.SET.modify)

        val titleView =
          aligner
            .zoom(ProgramNote.title, ProgramNotePropertiesInput.title.modify)
            .view(_.assign)

        val isPrivateView = aligner
          .zoom(ProgramNote.isPrivate, ProgramNotePropertiesInput.isPrivate.modify)
          .view(_.assign)

        val textView =
          aligner.zoom(ProgramNote.text, ProgramNotePropertiesInput.text.modify).view(_.orUnassign)

        <.div(
          ExploreStyles.ProgramNoteEditor,
          <.div(
            ExploreStyles.ProgramNoteTitle,
            <.div(
              LucumaPrimeStyles.FormColumn,
              FormInputTextView(
                id = "title".refined,
                label = "Title",
                value = titleView,
                validFormat = InputValidSplitEpi.nonEmptyString,
                disabled = props.userIsReadonlyCoi
              ).withMods(^.untypedRef := inputRef)
            ),
            CheckboxView(
              id = "is-private".refined,
              label = "Private",
              value = isPrivateView
            ).when(props.userIsStaff)
          ),
          MarkdownEditor(textView, props.userIsReadonlyCoi)
        )
    )
