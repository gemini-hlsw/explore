// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.refined.*
import monocle.Focus
import monocle.Lens

final case class ProgramNote(
  id:        ProgramNote.Id,
  title:     NonEmptyString,
  text:      Option[NonEmptyString],
  isPrivate: Boolean
) derives Eq,
      Decoder

object ProgramNote:
  type Id = lucuma.core.model.ProgramNote.Id
  val Id = lucuma.core.model.ProgramNote.Id

  val NewProgramNoteTitle: NonEmptyString = "<New Note>".refined

  def newProgramNote(id: ProgramNote.Id, title: NonEmptyString): ProgramNote =
    ProgramNote(id, title, None, false)

  val id: Lens[ProgramNote, ProgramNote.Id]           = Focus[ProgramNote](_.id)
  val title: Lens[ProgramNote, NonEmptyString]        = Focus[ProgramNote](_.title)
  val text: Lens[ProgramNote, Option[NonEmptyString]] = Focus[ProgramNote](_.text)
  val isPrivate: Lens[ProgramNote, Boolean]           = Focus[ProgramNote](_.isPrivate)
