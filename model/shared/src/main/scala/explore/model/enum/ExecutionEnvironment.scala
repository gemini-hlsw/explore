// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Enumerated
import lucuma.refined._

sealed abstract class ExecutionEnvironment(val suffix: Option[NonEmptyString])
    extends Product
    with Serializable
object ExecutionEnvironment {
  case object Development extends ExecutionEnvironment("DEV".refined[NonEmpty].some)
  case object Staging     extends ExecutionEnvironment("STG".refined[NonEmpty].some)
  case object Production  extends ExecutionEnvironment(none)

  /** @group Typeclass Instances */
  implicit val ExecutionEnvironmentEnumerated: Enumerated[ExecutionEnvironment] =
    Enumerated.of(Development, Staging, Production)
}
