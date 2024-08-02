// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.Tile.RenderInTitle
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTilesIds
import explore.model.Observation
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.react.common.*
import lucuma.react.common.ReactFnProps
import lucuma.react.markdown.ReactMarkdown
import lucuma.react.markdown.RehypePlugin
import lucuma.react.markdown.RemarkPlugin
import lucuma.react.primereact.*
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

case class NotesTile(
  obsId:         Observation.Id,
  notes:         View[Option[NonEmptyString]],
  renderInTitle: RenderInTitle
) extends ReactFnProps(NotesTile.component)

object NotesTile:
  private type Props = NotesTile

  val themeAttr = VdomAttr("data-theme")

  type Editing = Editing.Type
  object Editing extends NewType[Boolean]:
    inline def NotEditing = Editing(false)
    inline def InEdition  = Editing(true)
    inline def flip(e: Editing) = Editing(!e.value)

  def notesTile(obsId: Observation.Id, notes: View[Option[NonEmptyString]]) =
    Tile(
      ObsTabTilesIds.NotesId.id,
      s"Note for Observer",
      canMinimize = true,
      bodyClass = ExploreStyles.NotesTile
    )(NotesTile(obsId, notes, _))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(Editing.NotEditing)
      .useStateView("")
      .useEffectWithDepsBy((p, _, _) => p.notes.get.map(_.value).orEmpty) {
        (_, _, notesView) => r =>
          notesView.set(r)
      }
      .render { (props, editing, notesView) =>

        val editButton = Button("Edit",
                                severity = Button.Severity.Success,
                                disabled = editing.value.value,
                                icon = Icons.Edit,
                                onClick = editing.modState(Editing.flip)
        ).tiny.compact

        val save = editing.modState(Editing.flip) *>
          props.notes.set(NonEmptyString.from(notesView.get).toOption)

        val saveButton = Button(
          "Save",
          severity = Button.Severity.Success,
          icon = Icons.CloudArrowUp,
          onClick = save
        ).tiny.compact

        val editControls = <.div(ExploreStyles.NotesControls,
                                 HelpIcon("observer-notes.md".refined),
                                 if (editing.value.value) saveButton else editButton
        )

        val editor = FormInputTextAreaView("notes".refined, notesView)(
          ExploreStyles.NotesEditor
        )

        props.notes.get
          .map(_.value)
          .map { noteMd =>
            <.div(
              props.renderInTitle(editControls),
              ExploreStyles.ObserverNotes,
              themeAttr := "dark",
              if (editing.value.value) editor
              else
                ReactMarkdown(
                  content = noteMd,
                  clazz = ExploreStyles.HelpMarkdownBody,
                  remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
                  rehypePlugins = List(RehypePlugin.RehypeExternalLinks, RehypePlugin.RehypeKatex)
                )
            )
          }
          .getOrElse(
            <.div(ExploreStyles.NoNotes,
                  props.renderInTitle(editControls),
                  if (editing.value.value) editor else <.div("No notes available")
            )
          )
      }
