// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import clue.StreamingClient
import explore.model.enums.ObsAttachmentTypeMeta
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import queries.common.EnumQueriesGQL

case class Enums(enumMeta: Enums.Meta) {

  // The givens are apparently (probably) constructed lazily.
  // See https://alexn.org/blog/2022/05/11/implicit-vs-scala-3-given/
  // We want to fail immediately if there is a problem, so we'll reference
  // the enumerated givens here. Add any new enums to this list.
  Enumerated[ObsAttachmentType]
  Enumerated[ObsAttachmentTypeMeta]

  given Enumerated[ObsAttachmentTypeMeta] = Enumerated
    .from(
      enumMeta.obsAttachmentType.head._2,
      enumMeta.obsAttachmentType.tail.map(_._2).toList*
    )
    .withTag(_.tag.show)

  given Enumerated[ObsAttachmentType] = Enumerated
    .from(
      enumMeta.obsAttachmentType.head._1,
      enumMeta.obsAttachmentType.tail.map(_._1).toList*
    )
    .withTag(_.show)

  // This should be safe because the graphQL enums are built from the same
  // source as the metadata.
  given Display[ObsAttachmentType] =
    Display.by(t => enumMeta.obsAttachmentType(t).shortName,
               t => enumMeta.obsAttachmentType(t).longName
    )
}

object Enums {
  case class Meta(
    obsAttachmentType: Map[ObsAttachmentType, ObsAttachmentTypeMeta]
  )
  def load()(using StreamingClient[IO, ObservationDB]): IO[Enums] =
    for {
      obsAtt <- EnumQueriesGQL
                  .ObsAttachmentTypeMetaQuery[IO]
                  .query()
                  .map(_.obsAttachmentTypeMeta.map(o => (o.tag, o)).toMap)
    } yield Enums(Meta(obsAtt))
}
