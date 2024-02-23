// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import explore.UnderConstruction
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps

case class ProgramNotesTile() extends ReactFnProps(ProgramNotesTile.component)

object ProgramNotesTile:

  private type Props = ProgramNotesTile

  val component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>
      UnderConstruction()
    }
