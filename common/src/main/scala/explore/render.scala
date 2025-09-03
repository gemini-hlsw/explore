// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all.given
import explore.model.formats.durationHMS
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.TimingWindowEnd
import lucuma.core.model.TimingWindowRepeat
import lucuma.ui.format.*
import lucuma.ui.syntax.render.*
import lucuma.ui.utils.Render

object render:
  given Render[TimingWindowRepeat] = Render.by {
    case TimingWindowRepeat(period, None)                     =>
      React.Fragment(
        "repeat ",
        <.b("forever"),
        " with a period of ",
        <.b(durationHMS.reverseGet(period))
      )
    case TimingWindowRepeat(period, Some(n)) if n.value === 1 =>
      React.Fragment(s"repeat ", <.b("once"), " after ", <.b(durationHMS.reverseGet(period)))
    case TimingWindowRepeat(period, Some(n))                  =>
      React.Fragment(
        "repeat ",
        <.b(n.value, " times"),
        " with a period of ",
        <.b(durationHMS.reverseGet(period))
      )
  }

  given Render[TimingWindowEnd.After] = Render.by {
    case TimingWindowEnd.After(duration, None)         =>
      React.Fragment("for ", <.b(DurationLongFormatter(duration.toDuration)))
    case TimingWindowEnd.After(duration, Some(repeat)) =>
      React.Fragment(
        "for ",
        <.b(DurationLongFormatter(duration.toDuration)),
        ", ",
        repeat.renderVdom
      )
  }
