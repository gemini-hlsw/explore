// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.syntax.all.*
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.Site

trait ScienceModesSyntax:
  extension (scienceMode: ScienceMode)
    def fpuAlternative: Option[Either[GmosNorthFpu, GmosSouthFpu]] = scienceMode match
      case ScienceMode.GmosNorthLongSlit(basic, adv) =>
        adv.overrideFpu.getOrElse(basic.fpu).asLeft.some

      case ScienceMode.GmosSouthLongSlit(basic, adv) =>
        adv.overrideFpu.getOrElse(basic.fpu).asRight.some

    def siteFor: Site = scienceMode match
      case ScienceMode.GmosNorthLongSlit(_, _) => Site.GN
      case ScienceMode.GmosSouthLongSlit(_, _) => Site.GS

    def isCustomized: Boolean = scienceMode match
      case ScienceMode.GmosNorthLongSlit(_, advanced) =>
        advanced =!= ScienceModeAdvanced.GmosNorthLongSlit.Empty
      case ScienceMode.GmosSouthLongSlit(_, advanced) =>
        advanced =!= ScienceModeAdvanced.GmosSouthLongSlit.Empty

object scienceModes extends ScienceModesSyntax
