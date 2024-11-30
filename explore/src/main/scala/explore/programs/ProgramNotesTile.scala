// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.UnderConstruction

case class ProgramNotesTile() extends ReactFnProps(ProgramNotesTile.component)

object ProgramNotesTile:
  private type Props = ProgramNotesTile

  private val component = ScalaFnComponent[Props]: _ =>
    UnderConstruction()
