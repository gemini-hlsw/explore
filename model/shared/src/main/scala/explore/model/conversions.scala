// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift

trait conversions {
  val rvToRedshiftGet: RadialVelocity => Redshift =
    _.toRedshift.get

  val rvToRedshiftMod: (Redshift => Redshift) => RadialVelocity => RadialVelocity =
    // toRedshift should return a Some since RadialVelocity should be in the rance of +/_ c,
    // and Redshift.toRadialVelocity should always return a Some, too.
    modZ => rv => modZ(rv.toRedshift.get).toRadialVelocity.get

  val rvToARVGet: RadialVelocity => ApparentRadialVelocity =
    rvToRedshiftGet.andThen(_.toApparentRadialVelocity)

  val rvToARVMod: (
    ApparentRadialVelocity => ApparentRadialVelocity
  ) => RadialVelocity => RadialVelocity =
    modZ => rvToRedshiftMod(rs => modZ(rs.toApparentRadialVelocity).toRedshift)

}

object conversions extends conversions
