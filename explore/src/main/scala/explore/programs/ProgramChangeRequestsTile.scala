// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import explore.UnderConstruction
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps

case class ProgramChangeRequestsTile() extends ReactFnProps(ProgramChangeRequestsTile.component)

object ProgramChangeRequestsTile:

  private type Props = ProgramChangeRequestsTile

  def component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>
      UnderConstruction()
    }
