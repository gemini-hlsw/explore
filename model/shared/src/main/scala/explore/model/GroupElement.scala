// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Enums.EditType

// These case classes are only used for decoding the graphql response.
case class Grouping(
  group:    Group,
  elements: List[Either[Observation.Id, Group.Id]]
) derives Eq

object Grouping:
  private given Decoder[Either[Observation.Id, Group.Id]] =
    Decoder
      .instance(_.downField("group").get[Group.Id]("id").map(_.asRight))
      .orElse:
        Decoder.instance(_.downField("observation").get[Observation.Id]("id").map(_.asLeft))

  given Decoder[Grouping] = Decoder.instance: c =>
    for
      group    <- c.as[Group]
      elements <- c.get[List[Either[Observation.Id, Group.Id]]]("elements")
    yield Grouping(group, elements)

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

case class GroupUpdate(
  value:         ServerIndexed[Grouping],
  parentGroupId: Option[Group.Id],
  existence:     Existence,
  editType:      EditType
) derives Eq

object GroupUpdate:
  given Decoder[GroupUpdate] = Decoder.instance: c =>
    for
      value         <- c.get[Grouping]("value")
      parentGroupId <- c.downField("meta").get[Option[Group.Id]]("parentGroupId")
      parentIndex   <- c.downField("meta").get[NonNegShort]("parentIndex")
      existence     <- c.downField("meta").get[Existence]("existence")
      editType      <- c.get[EditType]("editType")
    yield GroupUpdate(ServerIndexed(value, parentIndex), parentGroupId, existence, editType)
