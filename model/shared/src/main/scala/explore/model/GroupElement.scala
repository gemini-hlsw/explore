// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Decoder
import io.circe.HCursor
import io.circe.refined.given
import lucuma.core.model.Group
import lucuma.core.model.Observation
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.Traversal

import GroupElement.given

case class GroupElement(value: Either[GroupObs, Grouping], parentGroupId: Option[Group.Id])
    derives Eq

object GroupElement:
  val value: Lens[GroupElement, Either[GroupObs, Grouping]] = Focus[GroupElement](_.value)

  val groupObs =
    value.andThen(Prism[Either[GroupObs, Grouping], GroupObs](_.left.toOption)(_.asLeft))

  val grouping = value.andThen(Prism[Either[GroupObs, Grouping], Grouping](_.toOption)(_.asRight))

  val parentGroupId: Lens[GroupElement, Option[Group.Id]] =
    Focus[GroupElement](_.parentGroupId)

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

object GroupObs:
  val groupIndex: Lens[GroupObs, NonNegShort] = Focus[GroupObs](_.groupIndex)

case class Grouping(
  id:              Group.Id,
  name:            Option[String],
  minimumRequired: Option[NonNegShort],
  elements:        List[Either[GroupObs, GroupingElement]],
  parentId:        Option[Group.Id],
  parentIndex:     NonNegShort
) derives Eq,
      Decoder

object Grouping:
  val elements: Lens[Grouping, List[Either[GroupObs, GroupingElement]]] =
    Focus[Grouping](_.elements)

  val elementsTraversal = Traversal.fromTraverse[List, Either[GroupObs, GroupingElement]]

case class GroupingElement(id: Group.Id, parentIndex: NonNegShort) derives Eq, Decoder
