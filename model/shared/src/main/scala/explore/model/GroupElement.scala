// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.HCursor
import io.circe.refined.given
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given
import monocle.Focus
import monocle.Lens
import monocle.Prism

import GroupElement.given

case class GroupElement(value: Either[GroupObs, Grouping], parentGroupId: Option[Group.Id])
    derives Eq

object GroupElement:
  val value: Lens[GroupElement, Either[GroupObs, Grouping]] = Focus[GroupElement](_.value)

  val groupObs =
    value.andThen(Prism[Either[GroupObs, Grouping], GroupObs](_.left.toOption)(_.asLeft))

  val grouping = value.andThen(Prism[Either[GroupObs, Grouping], Grouping](_.toOption)(_.asRight))

  val groupId = grouping.andThen(Grouping.id)

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
  val id: Lens[GroupObs, Observation.Id]      = Focus[GroupObs](_.id)
  val groupIndex: Lens[GroupObs, NonNegShort] = Focus[GroupObs](_.groupIndex)

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
  def isAnd: Boolean = minimumRequired.isEmpty

object Grouping:
  val id: Lens[Grouping, Group.Id] = Focus[Grouping](_.id)

  val name: Lens[Grouping, Option[NonEmptyString]] = Focus[Grouping](_.name)

  val elements: Lens[Grouping, List[Either[GroupObs, GroupingElement]]] =
    Focus[Grouping](_.elements)

  val minimumRequired: Lens[Grouping, Option[NonNegShort]] = Focus[Grouping](_.minimumRequired)

  val parentId: Lens[Grouping, Option[Group.Id]] = Focus[Grouping](_.parentId)
  val parentIndex: Lens[Grouping, NonNegShort]   = Focus[Grouping](_.parentIndex)

  val ordered: Lens[Grouping, Boolean] = Focus[Grouping](_.ordered)

  val minimumInterval: Lens[Grouping, Option[TimeSpan]] = Focus[Grouping](_.minimumInterval)
  val maximumInterval: Lens[Grouping, Option[TimeSpan]] = Focus[Grouping](_.maximumInterval)

case class GroupingElement(id: Group.Id, parentIndex: NonNegShort) derives Eq, Decoder
