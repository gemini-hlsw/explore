// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed trait ExecutionEnvironment extends Product with Serializable
object ExecutionEnvironment {
  case object Development extends ExecutionEnvironment
  case object Staging     extends ExecutionEnvironment
  case object Production  extends ExecutionEnvironment

  /** @group Typeclass Instances */
  implicit val ExecutionEnvironmentEnumerated: Enumerated[ExecutionEnvironment] =
    Enumerated.of(Development, Staging, Production)
}
