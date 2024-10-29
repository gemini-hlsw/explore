// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import explore.model.itc.ItcResult
import explore.model.itc.ItcTargetProblem
import monocle.Focus
import monocle.Lens
import explore.modes.InstrumentRow

case class BasicConfigAndItc(
  configuration: InstrumentRow,
  itcResult:     Option[EitherNec[ItcTargetProblem, ItcResult]]
) derives Eq

object BasicConfigAndItc:
  val configuration: Lens[BasicConfigAndItc, InstrumentRow] =
    Focus[BasicConfigAndItc](_.configuration)

  val itcResult: Lens[BasicConfigAndItc, Option[EitherNec[ItcTargetProblem, ItcResult]]] =
    Focus[BasicConfigAndItc](_.itcResult)
