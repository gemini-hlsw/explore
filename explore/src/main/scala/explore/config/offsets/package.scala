// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.*
import lucuma.core.util.Enumerated
import monocle.Focus
import monocle.Lens

sealed trait GridParams derives Eq:
  def gridType: GridType

case class RectangularParams(
  rows:  PosInt,
  cols:  PosInt,
  stepP: Angle,
  stepQ: Angle
) extends GridParams derives Eq:
  def gridType: GridType = GridType.Rectangular

object RectangularParams:
  val rows: Lens[RectangularParams, PosInt] = Focus[RectangularParams](_.rows)
  val cols: Lens[RectangularParams, PosInt] = Focus[RectangularParams](_.cols)
  val stepP: Lens[RectangularParams, Angle] = Focus[RectangularParams](_.stepP)
  val stepQ: Lens[RectangularParams, Angle] = Focus[RectangularParams](_.stepQ)

case class SpiralParams(
  size: Angle
) extends GridParams derives Eq:
  def gridType: GridType = GridType.Spiral

object SpiralParams:
  val size: Lens[SpiralParams, Angle] = Focus[SpiralParams](_.size)

case class RandomParams(
  size: Angle // area radius
) extends GridParams derives Eq:
  def gridType: GridType = GridType.Random

object RandomParams:
  val size: Lens[RandomParams, Angle] = Focus[RandomParams](_.size)

enum GridType(val tag: String) derives Enumerated:
  case Rectangular extends GridType("rectangular")
  case Spiral      extends GridType("spiral")
  case Random      extends GridType("random")
