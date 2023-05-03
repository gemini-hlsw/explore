// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.StreamingClient
import explore.model.enums.ObsAttachmentType
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB
import queries.common.EnumQueriesGQL

// Not using a case class so that `obsAttachments` is not accessible directly, only through the Enumerated.
class Enums(obsAttachments: NonEmptyList[ObsAttachmentType]):
  // The givens are apparently (probably) constructed lazily.
  // See https://alexn.org/blog/2022/05/11/implicit-vs-scala-3-given/
  // We want to fail immediately if there is a problem, so we'll reference
  // the enumerated givens here. Add any new enums to this list.
  Enumerated[ObsAttachmentType]

  given Enumerated[ObsAttachmentType] = Enumerated.fromNEL(obsAttachments).withTag(_.tag)

object Enums:
  def load()(using StreamingClient[IO, ObservationDB]): IO[Enums] =
    EnumQueriesGQL
      .ObsAttachmentTypeMetaQuery[IO]
      .query()
      .map(_.obsAttachmentTypeMeta)
      .map(NonEmptyList.fromList(_).toRight(new Throwable("Empty enum!")))
      .rethrow
      .map(new Enums(_))
