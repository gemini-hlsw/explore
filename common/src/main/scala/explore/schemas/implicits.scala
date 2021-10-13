// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.data.syntax._
import lucuma.core.math._
import lucuma.core.model._
import lucuma.schemas.ObservationDB.Types._

import java.math.MathContext

import UserPreferencesDB.Types.ExploreResizableWidthInsertInput

// TODO Move to lucuma-schemas
object implicits {
  implicit class MagnitudeOps(m: Magnitude) {
    def toCreateInput: MagnitudeCreateInput =
      MagnitudeCreateInput(m.band,
                           m.value.toDoubleValue,
                           m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                           m.system.assign
      )
  }

  implicit class CatalogIdOps(cid: CatalogId) {
    def toInput: CatalogIdInput =
      CatalogIdInput(cid.catalog, cid.id.value)
  }

  implicit class RightAscensionOps(ra: RightAscension) {
    def toInput: RightAscensionInput =
      RightAscensionInput(microarcseconds = ra.toAngle.toMicroarcseconds.assign)
  }

  implicit class DeclinationOps(dec: Declination) {
    def toInput: DeclinationInput =
      DeclinationInput(microarcseconds = dec.toAngle.toMicroarcseconds.assign)
  }

  implicit class ProperMotionOps(pm: ProperMotion) {
    def toInput: ProperMotionInput =
      ProperMotionInput(
        ra = ProperMotionComponentInput(microarcsecondsPerYear = pm.ra.μasy.value.assign),
        dec = ProperMotionComponentInput(microarcsecondsPerYear = pm.dec.μasy.value.assign)
      )
  }

  implicit class RadialVelocityOps(rv: RadialVelocity) {
    def toInput: RadialVelocityInput =
      RadialVelocityInput(metersPerSecond = rv.rv.value.assign)
  }

  implicit class ParallaxOps(p: Parallax) {
    def toInput: ParallaxModelInput =
      ParallaxModelInput(microarcseconds = p.μas.value.value.assign)
  }

  implicit class SiderealTargetOps(sidereal: SiderealTarget) {
    def toCreateInput: CreateSiderealInput =
      CreateSiderealInput(
        name = sidereal.name,
        catalogId = sidereal.tracking.catalogId.map(_.toInput).orIgnore,
        ra = sidereal.tracking.baseCoordinates.ra.toInput,
        dec = sidereal.tracking.baseCoordinates.dec.toInput,
        epoch = Epoch.fromString.reverseGet(sidereal.tracking.epoch).assign,
        properMotion = sidereal.tracking.properMotion.map(_.toInput).orIgnore,
        radialVelocity = sidereal.tracking.radialVelocity.map(_.toInput).orIgnore,
        parallax = sidereal.tracking.parallax.map(_.toInput).orIgnore,
        magnitudes = sidereal.magnitudes.values.toList.map(_.toCreateInput).assign
      )
  }

  implicit class NonsiderealTargetOps(nonsidereal: NonsiderealTarget) {
    def toCreateInput: CreateNonsiderealInput =
      CreateNonsiderealInput(
        name = nonsidereal.name,
        keyType = nonsidereal.ephemerisKey.keyType,
        des = nonsidereal.ephemerisKey.des,
        magnitudes = nonsidereal.magnitudes.values.toList.map(_.toCreateInput).assign
      )
  }

  implicit class TargetOps(target: Target) {
    def toCreateInput: CreateTargetInput = target match {
      case sidereal @ SiderealTarget(_, _, _)       =>
        CreateTargetInput(sidereal = sidereal.toCreateInput.assign)
      case nonsidereal @ NonsiderealTarget(_, _, _) =>
        CreateTargetInput(nonsidereal = nonsidereal.toCreateInput.assign)
    }
  }

  implicit def widthUpsertInput(w: WidthUpsertInput): ExploreResizableWidthInsertInput =
    ExploreResizableWidthInsertInput(
      w.section.value.assign,
      w.user.toString.assign,
      w.width.assign
    )
}
