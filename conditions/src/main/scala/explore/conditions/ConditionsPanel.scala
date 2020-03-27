// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import gem.Observation

/*
query {
  conditions {
    observation_id
  }
}

subscription MyQuery {
  conditions(where: {observation_id: {_eq: "368e5b67-6c1e-4d77-8547-ef16766802fd"}}) {
    observation_id
    cloud_cover
    image_quality
    sky_background
    water_vapor
  }
}

mutation {
  insert_conditions(objects: [{
    observation_id: "368e5b67-6c1e-4d77-8547-ef16766802fe",
    cloud_cover: "Any",
    image_quality: "Any",
    sky_background: "Any",
    water_vapor: "Any"
  }]) {
    affected_rows
  }
}

mutation {
  update_conditions(_set: {
    cloud_cover: "Percent50",
    image_quality: "Any",
    sky_background: "Any",
    water_vapor:"Any"
  }, where: {
    observation_id: {
      _eq: "368e5b67-6c1e-4d77-8547-ef16766802fd"
    }
  }) {
    affected_rows
  }
}

*/

final case class ConditionsPanel(
  observationId: Observation.Id
) extends ReactProps {
  @inline override def render: VdomElement = ConditionsPanel.component(this)
}

object ConditionsPanel {
  type Props = ConditionsPanel



  protected val component =
    ScalaComponent
      .builder[ConditionsPanel]("ConditionsPanel")
      .render { _ =>
        <.div(

        )
      }
      .build
}
