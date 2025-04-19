// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift

trait conversions {
  val rvToRedshiftGet: Option[RadialVelocity] => Option[Redshift] =
    _.flatMap(_.toRedshift)

  val rvToRedshiftMod
    : (Option[Redshift] => Option[Redshift]) => Option[RadialVelocity] => Option[RadialVelocity] =
    modZ => rv => modZ(rv.flatMap(_.toRedshift)).flatMap(_.toRadialVelocity)

  val rvToARVGet: Option[RadialVelocity] => Option[ApparentRadialVelocity] =
    rvToRedshiftGet.andThen(_.map(_.toApparentRadialVelocity))

  val rvToARVMod: (
    Option[ApparentRadialVelocity] => Option[ApparentRadialVelocity]
  ) => Option[RadialVelocity] => Option[RadialVelocity] =
    modZ => rvToRedshiftMod(rsOpt => modZ(rsOpt.map(_.toApparentRadialVelocity)).map(_.toRedshift))

}

object conversions extends conversions
