// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

sealed trait Target
object Target {
  final case object M81 extends Target
  final case object M51 extends Target
}
