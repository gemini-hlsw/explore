// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.Existence

import scala.annotation.targetName

// These case classes are only used for decoding the graphql response.
case class GroupWithChildren(
  group:    Group,
  children: List[GroupWithChildren.Child]
) derives Eq

object GroupWithChildren:
  type Child = ServerIndexed[Either[Observation.Id, Group.Id]]

  @targetName("obsIdDecoder")
  private given Decoder[ServerIndexed[Observation.Id]] =
    Decoder.instance: c =>
      for
        obsId       <- c.get[Observation.Id]("id")
        parentIndex <- c.get[NonNegShort]("groupIndex")
      yield ServerIndexed(obsId, parentIndex)

  @targetName("groupIdDecoder")
  private given Decoder[ServerIndexed[Group.Id]] =
    Decoder.instance: c =>
      for
        groupId     <- c.get[Group.Id]("id")
        parentIndex <- c.get[NonNegShort]("parentIndex")
      yield ServerIndexed(groupId, parentIndex)

  given Decoder[Child] =
    Decoder.instance: c =>
      c.downField("observation")
        .as[ServerIndexed[Observation.Id]]
        .map(_.map(_.asLeft))
        .orElse(c.downField("group").as[ServerIndexed[Group.Id]].map(_.map(_.asRight)))

  given Decoder[GroupWithChildren] = Decoder.instance: c =>
    for
      group    <- c.get[Group]("group")
      children <- c.downField("groupChildren").get[List[Child]]("elements")
    yield GroupWithChildren(group, children)

case class GroupElement(
  value:            Either[Observation.Id, GroupWithChildren],
  indexInRootGroup: Option[NonNegShort] // Only defined if element is in root group
) derives Eq

object GroupElement:
  given Decoder[GroupElement] = Decoder.instance: c =>
    given Decoder[Either[Observation.Id, GroupWithChildren]] =
      Decoder
        .instance(_.as[GroupWithChildren].map(_.asRight))
        .orElse:
          Decoder.instance(_.downField("observation").get[Observation.Id]("id").map(_.asLeft))

    for
      value         <- c.as[Either[Observation.Id, GroupWithChildren]]
      parentIndex   <- c.get[NonNegShort]("parentIndex")
      parentGroupId <- c.get[Option[Group.Id]]("parentGroupId")
    yield GroupElement(value, Option.when(parentGroupId.isEmpty)(parentIndex))

// Necessary because we receive events with null values when they are on the root group.
case class GroupUpdatePayload(
  value:         ServerIndexed[Group],
  parentGroupId: Option[Group.Id],
  existence:     Existence
) derives Eq

case class GroupUpdate(
  payload:  Option[GroupUpdatePayload],
  editType: EditType
) derives Eq

object GroupUpdate:
  given Decoder[GroupUpdate] = Decoder.instance: c =>
    for
      editType <- c.get[EditType]("editType")
      payload  <- c.get[Option[Group]]("value")
                    .flatMap:
                      _.map: grouping =>
                        for
                          parentGroupId <- c.downField("meta").get[Option[Group.Id]]("parentId")
                          parentIndex   <- c.downField("meta").get[NonNegShort]("parentIndex")
                          existence     <- c.downField("meta").get[Existence]("existence")
                        yield GroupUpdatePayload(
                          ServerIndexed(grouping, parentIndex),
                          parentGroupId,
                          existence
                        ).some
                      .getOrElse(none.asRight)
    yield GroupUpdate(payload, editType)
