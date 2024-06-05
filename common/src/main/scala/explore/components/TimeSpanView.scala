// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.model.syntax.all.toHoursMinutes
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps

case class TimeSpanView(timespan: TimeSpan, modifiers: Seq[TagMod] = Seq.empty)
    extends ReactFnProps(TimeSpanView.component):
  def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)

  // You can add content to the button as children without this method, but if you want
  // to add event handlers or other attributes, you'll need to use the modifiers.
  def withMods(mods: TagMod*) = addModifiers(mods)

object TimeSpanView:

  def component = ScalaFnComponent[TimeSpanView]: props =>
    val ts = props.timespan

    <.span(
      props.modifiers.toTagMod,
      ^.title := s"${ts.toHoursPart} hours, ${ts.toMinutesPart} minutes, ${ts.toSecondsPart} seconds",
      ts.toHoursMinutes
    )
