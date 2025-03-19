// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.MarkdownEditor
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import explore.model.enums.TileSizeState
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.refined.*

object NotesTile:

  def notesTile(notes: View[Option[NonEmptyString]]): Tile[Unit] =
    Tile(
      ObsTabTileIds.NotesId.id,
      s"Note for Observer",
      bodyClass = ExploreStyles.NotesTile
    )(_ => NotesTileBody(notes), (_, tileSize) => NotesTileTitle(notes, tileSize))

case class NotesTileBody(
  notes: View[Option[NonEmptyString]]
) extends ReactFnProps(NotesTileBody.component)

object NotesTileBody:
  private type Props = NotesTileBody

  val themeAttr = VdomAttr("data-theme")

  private val component =
    ScalaFnComponent[Props]: props =>
      MarkdownEditor(props.notes)

case class NotesTileTitle(
  notes:    View[Option[NonEmptyString]],
  tileSize: TileSizeState
) extends ReactFnProps(NotesTileTitle.component)

object NotesTileTitle:
  private type Props = NotesTileTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      Option.unless(props.tileSize === TileSizeState.Minimized)(
        HelpIcon("observer-notes.md".refined)
      )
