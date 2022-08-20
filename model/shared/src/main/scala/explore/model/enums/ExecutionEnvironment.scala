// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Enumerated
import lucuma.refined._

enum ExecutionEnvironment(val suffix: Option[NonEmptyString]):
  case Development extends ExecutionEnvironment("DEV".refined[NonEmpty].some)
  case Staging     extends ExecutionEnvironment("STG".refined[NonEmpty].some)
  case Production  extends ExecutionEnvironment(none)

object ExecutionEnvironment:
  /** @group Typeclass Instances */
  given Enumerated[ExecutionEnvironment] =
    Enumerated.from(Development, Staging, Production).withTag(_.suffix.foldMap(_.value))
