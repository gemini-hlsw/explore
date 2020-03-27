// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.elements.button._
import react.semanticui.colors._
import react.common._
import crystal.react.io.implicits._
import explore.model._
import scala.scalajs.js
import js.JSConverters._
import js.UndefOr._
import cats.effect._
import explore.model.AppStateIO._
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

  private def renderButton(forTarget: Target, selected: Option[Target]) = {
    val color = selected.filter(_ == forTarget).map(_ => Blue).orUndefined
    Button(onClick = AppState.views.target.set(Some(forTarget)), color = color)(forTarget.toString)
  }

  private def retrievePersons(): IO[Unit] =
    for {
      persons <- AppState.actions.persons.query()
      _ = println(persons)
      _ <- AppState.views.persons.set(persons)
    } yield ()

  protected val component =
    ScalaComponent
      .builder[ConditionsPanel]("ConditionsPanel")
      .render { _ =>
        <.div(
          <.div(
            Button("IQ"),
            Button(color = Blue)("Button", "Btn"),
            Button("Button", "Dec")
          ),
          AppState.views.target.streamRender(selected =>
            <.div(
              List(Target.M81, Target.M51).toTagMod(target => renderButton(target, selected))
            )
          ),
          Button(onClick = retrievePersons())("GraphQL Test")
        )
      }
      .build
}
