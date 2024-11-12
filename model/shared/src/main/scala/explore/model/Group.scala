// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.syntax.all.*
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given
import monocle.Focus
import monocle.Lens

case class Group(
  id:              Group.Id,
  name:            Option[NonEmptyString],
  minimumRequired: Option[NonNegShort],
  minimumInterval: Option[TimeSpan],
  maximumInterval: Option[TimeSpan],
  ordered:         Boolean,
  system:          Boolean,
  parentId:        Option[Group.Id],
  parentIndex:     NonNegShort
) derives Eq,
      Decoder:
  def isAnd: Boolean = minimumRequired.isEmpty

object Group:
  type Id = lucuma.core.model.Group.Id
  val Id = lucuma.core.model.Group.Id

  val id: Lens[Group, Group.Id]                         = Focus[Group](_.id)
  val name: Lens[Group, Option[NonEmptyString]]         = Focus[Group](_.name)
  val minimumInterval: Lens[Group, Option[TimeSpan]]    = Focus[Group](_.minimumInterval)
  val maximumInterval: Lens[Group, Option[TimeSpan]]    = Focus[Group](_.maximumInterval)
  val minimumRequired: Lens[Group, Option[NonNegShort]] = Focus[Group](_.minimumRequired)
  val ordered: Lens[Group, Boolean]                     = Focus[Group](_.ordered)
  val system: Lens[Group, Boolean]                      = Focus[Group](_.system)
  val parentId: Lens[Group, Option[Group.Id]]           = Focus[Group](_.parentId)
  val parentIndex: Lens[Group, NonNegShort]             = Focus[Group](_.parentIndex)
