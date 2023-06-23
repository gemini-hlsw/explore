// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB.Enums.Existence
import monocle.Focus
import monocle.Lens

case class ProgramInfo(id: Program.Id, name: Option[NonEmptyString], deleted: Boolean) derives Eq

object ProgramInfo:
  val id: Lens[ProgramInfo, Program.Id]               = Focus[ProgramInfo](_.id)
  val name: Lens[ProgramInfo, Option[NonEmptyString]] = Focus[ProgramInfo](_.name)
  val deleted: Lens[ProgramInfo, Boolean]             = Focus[ProgramInfo](_.deleted)

  given Decoder[ProgramInfo] = Decoder.instance(c =>
    for {
      id        <- c.get[Program.Id]("id")
      name      <- c.get[Option[NonEmptyString]]("name")
      existence <- c.get[Existence]("existence")
    } yield ProgramInfo(id, name, existence === Existence.Deleted)
  )
