// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.tree.KeyedIndexedTree.Index
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.model.Group
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given

import io.circe.JsonObject
import lucuma.typed.react.reactStrings.group

// type GroupElement = ServerIndexed[Either[Observation.Id, Grouping]]

// These case classes are only used for decoding the graphql response.
case class GroupElement(
  value:         ServerIndexed[Either[Observation.Id, Grouping]],
  parentGroupId: Option[Group.Id]
) derives Eq

object GroupElement:
  given Decoder[GroupElement] = Decoder.instance: c =>
    given Decoder[Either[Observation.Id, Grouping]] =
      Decoder
        .instance(_.get[Grouping]("group").map(_.asRight))
        .orElse:
          Decoder.instance(_.downField("observation").get[Observation.Id]("id").map(_.asLeft))

    for
      value         <- c.as[Either[Observation.Id, Grouping]]
      parentIndex   <- c.get[NonNegShort]("parentIndex")
      parentGroupId <- c.get[Option[Group.Id]]("parentGroupId")
    yield GroupElement(ServerIndexed(value, parentIndex), parentGroupId)

    // for
    //   value       <- c.get[Grouping]("group")
    //                    .map(_.asRight)
    //                    .orElse(c.downField("observation").get[Observation.Id]("id").map(_.asLeft))
    //   parentIndex <- c.get[NonNegShort]("parentIndex")
    // yield GroupElement(value, parentIndex)

case class Grouping(
  group:    Group,
  elements: List[Either[Observation.Id, Group.Id]]
) derives Eq,
      Decoder

object Grouping:
  private given Decoder[Either[Observation.Id, Group.Id]] =
    Decoder
      .instance(_.downField("group").get[Group.Id]("id").map(_.asRight))
      .orElse:
        Decoder.instance(_.downField("observation").get[Observation.Id]("id").map(_.asLeft))
