// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import explore.model.itc.ItcResult
import explore.model.itc.ItcTargetProblem
import lucuma.schemas.model.BasicConfiguration
import monocle.Focus
import monocle.Lens

case class BasicConfigAndItc(
  configuration: BasicConfiguration,
  itcResult:     Option[EitherNec[ItcTargetProblem, ItcResult]]
) derives Eq

object BasicConfigAndItc:
  val configuration: Lens[BasicConfigAndItc, BasicConfiguration] =
    Focus[BasicConfigAndItc](_.configuration)

  val itcResult: Lens[BasicConfigAndItc, Option[EitherNec[ItcTargetProblem, ItcResult]]] =
    Focus[BasicConfigAndItc](_.itcResult)
