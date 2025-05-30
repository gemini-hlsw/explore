// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.ui

import lucuma.core.enums.Partner

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object PartnerFlags {
  @js.native
  @JSImport("/images/flags/argentina-flag-icon-32.png", JSImport.Default)
  val arSmall: String = js.native

  @js.native
  @JSImport("/images/flags/brazil-flag-icon-32.png", JSImport.Default)
  val brSmall: String = js.native

  @js.native
  @JSImport("/images/flags/canada-flag-icon-32.png", JSImport.Default)
  val caSmall: String = js.native

  @js.native
  @JSImport("/images/flags/chile-flag-icon-32.png", JSImport.Default)
  val clSmall: String = js.native

  @js.native
  @JSImport("/images/flags/south-korea-flag-icon-32.png", JSImport.Default)
  val krSmall: String = js.native

  @js.native
  @JSImport("/images/flags/uh.png", JSImport.Default)
  val uhSmall: String = js.native

  @js.native
  @JSImport("/images/flags/united-states-of-america-flag-icon-32.png", JSImport.Default)
  val usSmall: String = js.native

  def smallFlag(partner: Partner): String = partner match {
    case Partner.AR => arSmall
    case Partner.BR => brSmall
    case Partner.CA => caSmall
    case Partner.CL => clSmall
    case Partner.KR => krSmall
    case Partner.UH => uhSmall
    case Partner.US => usSmall
  }
}
