// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.generic.semiauto.*
import lucuma.core.model.TimingWindow
import monocle.Focus

case class SchedulingGroup(timingWindows: List[TimingWindow], obsIds: ObsIdSet) derives Eq

object SchedulingGroup:
  val timingWindows = Focus[SchedulingGroup](_.timingWindows)
  val obsIds        = Focus[SchedulingGroup](_.obsIds)

  def fromTuple(tuple: (ObsIdSet, List[TimingWindow])): SchedulingGroup =
    SchedulingGroup(tuple._2, tuple._1)
