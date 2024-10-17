// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyChain
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.itc.ItcCcd
import lucuma.react.fa.*
import lucuma.react.floatingui.syntax.*
import lucuma.ui.syntax.all.given

extension (role: Option[CalibrationRole])
  // Icon to indicate a field is required to do ITC calculations
  def renderRequiredForITCIcon: TagMod =
    <.span(
      LayeredIcon(fixedWidth = true, clazz = ExploreStyles.WarningIcon)(
        Icons.StarExclamation.withSize(IconSize.X1),
        TextLayer("ITC", clazz = ExploreStyles.RequiredForItcText, inverse = true)
      )
    ).withTooltip("Required for ITC")

def formatCcds(
  ccds:      Option[NonEmptyChain[ItcCcd]],
  extractor: NonEmptyChain[ItcCcd] => String
): String =
  ccds.fold("-")(extractor)
