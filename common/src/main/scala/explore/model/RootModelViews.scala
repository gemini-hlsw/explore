// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import crystal.Pot
import crystal.react.ThrottlingView
import crystal.react.View
import monocle.Focus
import monocle.Lens

case class RootModelViews(
  rootModel:        View[RootModel],
  programSummaries: ThrottlingView[Pot[ProgramSummaries]]
):
  lazy val programSummariesValue: Pot[ProgramSummaries] = programSummaries.throttlerView.get

object RootModelViews:
  val rootModel: Lens[RootModelViews, View[RootModel]]                              =
    Focus[RootModelViews](_.rootModel)
  val programSummaries: Lens[RootModelViews, ThrottlingView[Pot[ProgramSummaries]]] =
    Focus[RootModelViews](_.programSummaries)
