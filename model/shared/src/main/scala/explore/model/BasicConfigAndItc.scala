// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.syntax.all.*
import explore.model.itc.ItcResult
import explore.model.itc.ItcTargetProblem
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import lucuma.schemas.model.BasicConfiguration
import monocle.Focus
import monocle.Lens

case class BasicConfigAndItc(
  configuration: InstrumentRow,
  itcResult:     Option[EitherNec[ItcTargetProblem, ItcResult]]
) derives Eq:
  def toBasicConfiguration: Option[BasicConfiguration] =
    configuration match
      case GmosNorthSpectroscopyRow(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosNorthLongSlit(grating, filter, fpu, cw).some
      case GmosSouthSpectroscopyRow(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosSouthLongSlit(grating, filter, fpu, cw).some
      case _                                                              => none

object BasicConfigAndItc:
  val configuration: Lens[BasicConfigAndItc, InstrumentRow] =
    Focus[BasicConfigAndItc](_.configuration)

  val itcResult: Lens[BasicConfigAndItc, Option[EitherNec[ItcTargetProblem, ItcResult]]] =
    Focus[BasicConfigAndItc](_.itcResult)
