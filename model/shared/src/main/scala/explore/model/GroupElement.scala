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
import io.circe.HCursor
import io.circe.refined.given
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given

import GroupElement.given

// These case classes are only used for decoding the graphql response. Used case classes are in GroupTree.scala
case class GroupElement(value: Either[GroupObs, Grouping], parentGroupId: Option[Group.Id])
    derives Eq

object GroupElement:

  given Decoder[GroupElement] = Decoder.instance(c =>
    for {
      value         <- groupObsOr(_.get[Grouping]("group"))(c)
      parentGroupId <- c.get[Option[Group.Id]]("parentGroupId")
    } yield GroupElement(
      value,
      parentGroupId
    )
  )

  given Decoder[Either[GroupObs, GroupingElement]] =
    Decoder.instance(groupObsOr(_.get[GroupingElement]("group")))

  private def groupObsOr[B](f: HCursor => Decoder.Result[B]) = (c: HCursor) =>
    f(c).map(_.asRight).orElse(c.get[GroupObs]("observation").map(_.asLeft))

case class GroupObs(id: Observation.Id, groupIndex: NonNegShort) derives Eq, Decoder

case class Grouping(
  id:              Group.Id,
  name:            Option[NonEmptyString],
  minimumRequired: Option[NonNegShort],
  elements:        List[Either[GroupObs, GroupingElement]],
  parentId:        Option[Group.Id],
  parentIndex:     NonNegShort,
  minimumInterval: Option[TimeSpan],
  maximumInterval: Option[TimeSpan],
  ordered:         Boolean
) derives Eq,
      Decoder:
  def toGroupTreeGroup: GroupTree.Group =
    GroupTree.Group(id, name, minimumRequired, minimumInterval, maximumInterval, ordered)

  def toIndex: GroupTree.Index = Index(parentId.map(_.asRight), parentIndex)

case class GroupingElement(id: Group.Id, parentIndex: NonNegShort) derives Eq, Decoder
