// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.enums.TileSizeState
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.react.common.*
import lucuma.react.markdown.ReactMarkdown
import lucuma.react.markdown.RehypePlugin
import lucuma.react.markdown.RemarkPlugin
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import monocle.Focus
import monocle.Lens

object NotesTile:

  def notesTile(obsId: Observation.Id, notes: View[Option[NonEmptyString]]) =
    Tile(
      ObsTabTileIds.NotesId.id,
      s"Note for Observer",
      NotesTileState(notes.get.foldMap(_.value), Editing.NotEditing),
      bodyClass = ExploreStyles.NotesTile
    )(NotesTileBody(obsId, notes, _), NotesTileTitle(obsId, notes, _, _))

case class NotesTileState(notes: String, editing: Editing)

object NotesTileState:
  val notes: Lens[NotesTileState, String]    = Focus[NotesTileState](_.notes)
  val editing: Lens[NotesTileState, Editing] = Focus[NotesTileState](_.editing)

case class NotesTileBody(
  obsId: Observation.Id,
  notes: View[Option[NonEmptyString]],
  state: View[NotesTileState]
) extends ReactFnProps(NotesTileBody.component)

type Editing = Editing.Type
object Editing extends NewType[Boolean]:
  inline def NotEditing       = Editing(false)
  inline def InEdition        = Editing(true)
  inline def flip(e: Editing) = Editing(!e.value)

object NotesTileBody:
  private type Props = NotesTileBody

  val themeAttr = VdomAttr("data-theme")

  private val component =
    ScalaFnComponent[Props] { props =>
      val notesView = props.state.zoom(NotesTileState.notes)
      val editing   = props.state.zoom(NotesTileState.editing)

      val editor = FormInputTextAreaView("notes".refined, notesView)(
        ExploreStyles.NotesEditor
      )

      props.notes.get
        .map(_.value)
        .map(noteMd =>
          <.div(
            ExploreStyles.ObserverNotes,
            themeAttr := "dark",
            if (editing.get.value) editor
            else
              ReactMarkdown(
                content = noteMd,
                clazz = ExploreStyles.HelpMarkdownBody,
                remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
                rehypePlugins = List(RehypePlugin.RehypeExternalLinks, RehypePlugin.RehypeKatex)
              )
          )
        )
        .getOrElse(
          <.div(ExploreStyles.NoNotes,
                if (editing.get.value) editor else <.div("No notes available")
          )
        )
    }

case class NotesTileTitle(
  obsId:    Observation.Id,
  notes:    View[Option[NonEmptyString]],
  state:    View[NotesTileState],
  tileSize: TileSizeState
) extends ReactFnProps(NotesTileTitle.component)

object NotesTileTitle:
  private type Props = NotesTileTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      val editing = props.state.zoom(NotesTileState.editing)

      val notesView = props.state.zoom(NotesTileState.notes)

      val editButton = Button("Edit",
                              severity = Button.Severity.Success,
                              disabled = editing.get.value,
                              icon = Icons.Edit,
                              onClick = editing.mod(Editing.flip)
      ).tiny.compact

      val save = editing.mod(Editing.flip) *>
        props.notes.set(NonEmptyString.from(notesView.get).toOption)

      val saveButton = Button(
        "Save",
        severity = Button.Severity.Success,
        icon = Icons.CloudArrowUp,
        onClick = save
      ).tiny.compact

      if (props.tileSize === TileSizeState.Minimized) <.div(ExploreStyles.NotesControls)
      else
        <.div(ExploreStyles.NotesControls,
              HelpIcon("observer-notes.md".refined),
              if (editing.get.value) saveButton else editButton
        )
