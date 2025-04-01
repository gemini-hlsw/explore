// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.syntax.all.*
import explore.model.itc.ItcResult
import explore.model.itc.ItcTargetProblem
import explore.modes.InstrumentConfig
import lucuma.core.enums.Instrument
import lucuma.schemas.model.BasicConfiguration
import monocle.Focus
import monocle.Lens

case class InstrumentConfigAndItcResult(
  instrumentConfig: InstrumentConfig,
  itcResult:        Option[EitherNec[ItcTargetProblem, ItcResult]]
) derives Eq:
  def instrument: Instrument = instrumentConfig.instrument

  def toBasicConfiguration: Option[BasicConfiguration] =
    instrumentConfig match
      case InstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosNorthLongSlit(grating, filter, fpu, cw).some
      case InstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosSouthLongSlit(grating, filter, fpu, cw).some
      case _                                                                            => none

object InstrumentConfigAndItcResult:
  val configuration: Lens[InstrumentConfigAndItcResult, InstrumentConfig] =
    Focus[InstrumentConfigAndItcResult](_.instrumentConfig)

  val itcResult
    : Lens[InstrumentConfigAndItcResult, Option[EitherNec[ItcTargetProblem, ItcResult]]] =
    Focus[InstrumentConfigAndItcResult](_.itcResult)
