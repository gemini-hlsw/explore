// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.starwars

import cats.implicits._
import cats.effect._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import crystal.react.io.implicits._
import diode.data._
import diode.react.ReactPot._
import explore.model.AppStateIO._
import monocle.macros._
import react.semanticui.modules.dropdown._
import react.semanticui.elements.icon.Icon
import io.circe._
import scala.scalajs.js.JSConverters._

object EpisodeHero {

  sealed trait Episode
  object Episode {
    case object NEWHOPE extends Episode
    case object EMPIRE extends Episode
    case object JEDI extends Episode

    val all        = List(NEWHOPE, EMPIRE, JEDI)
    val fromString = all.map(e => e.toString -> e).toMap
  }

  @Lenses
  final case class State(episode: Option[Episode] = None, hero: Pot[String] = Empty)

  class Backend($ : BackendScope[Unit, State]) {

    private def queryDoc(episode: Episode) = s"""
      {
        hero(episode: $episode) {
          name
        }
      }"""

    def query(episode: Episode): IO[Unit] =
      for {
        _ <- $.setStateIO(State(episode.some, Pending()))
        //hero <- AppState.Clients.starWars.query[Map[String, String], String](queryDoc, Map("episode" -> episode.toString))
        //hero <- AppState.Clients.starWars.query[Json, String](queryDoc, Json.obj("episode" -> Json.fromString(episode.toString)))
        json <- AppState.clients.starWars.query[Json, Json](queryDoc(episode), Json.obj())
        _ <- $.modStateIO(
          State.hero.set(
            Ready(json.hcursor.downField("hero").downField("name").as[String].getOrElse("???"))
          )
        )
      } yield ()

    // val onChange: (ReactEvent, Dropdown.DropdownProps) => Callback =
    // (_, p) => query(Episode.fromString(p.value.asInstanceOf[String])))

    val onClickItem: (ReactEvent, DropdownItem.DropdownItemProps) => Callback =
      (_, p) => query(Episode.fromString(p.value.asInstanceOf[String]))

    def render(state: State) =
      <.div(
        Dropdown(
          placeholder = "Select episode...",
          selection   = true,
          value       = state.episode.map(_.toString).orUndefined,
          options = Episode.all.map { e =>
            DropdownItem(
              text     = e.toString,
              value    = e.toString,
              onClickE = onClickItem
            )(
              ^.key := e.toString
            )
          }
          // onChange = onChange
        ),
        ":",
        state.hero.renderPending(_ => Icon(name = "spinner", loading = true)),
        state.hero.render(h => h: VdomNode)
      )
  }

  private val component =
    ScalaComponent
      .builder[Unit]("EpisodeHero")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply() = component()
}
