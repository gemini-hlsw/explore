// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import crystal.Pot
import explore.components.ui.ExploreStyles
import explore.model.PartnerAllocationList
import explore.model.ProgramTimes
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps

case class ProgramDetailsTile(
  allocations:  PartnerAllocationList,
  programTimes: Pot[ProgramTimes]
) extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:
  private type Props = ProgramDetailsTile

  val component = ScalaFnComponent[Props]: props =>
    <.div(ExploreStyles.ProgramDetailsTile)(
      <.div,
      <.div(
        TimeAwardTable(props.allocations),
        TimeAccountingTable(props.programTimes)
      ),
      <.div
    )
