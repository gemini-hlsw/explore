// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq
import react.common.style.Css

sealed trait Theme extends Product with Serializable {
  def clazz: Css
}

object Theme {
  case object Dark  extends Theme {
    val clazz = Css("dark-theme")
  }
  case object Light extends Theme {
    val clazz = Css("light-theme")
  }

  /** @group Typeclass Instances */
  given Eq[Theme] = Eq.by(_.clazz)
}
