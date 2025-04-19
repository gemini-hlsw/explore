// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  def apply(notes: View[Option[NonEmptyString]], hidden: Boolean): Tile[Unit] =
    Tile(
      ObsTabTileIds.NotesId.id,
      s"Note for Observer",
      bodyClass = ExploreStyles.NotesTile,
      hidden = hidden
    )(_ => MarkdownEditor(notes),
      (_, tileSize) =>
        Option.unless(tileSize === TileSizeState.Minimized)(
          HelpIcon("observer-notes.md".refined)
        )
    )
