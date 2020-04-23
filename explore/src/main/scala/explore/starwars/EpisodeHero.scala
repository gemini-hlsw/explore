// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.starwars

import explore.implicits._
import cats.implicits._
import cats.effect._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import io.circe._
import crystal.react.implicits._
import diode.data._
import diode.react.ReactPot._
import monocle.macros._
import react.semanticui.modules.dropdown._
import react.semanticui.elements.icon.Icon
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

  type Props = AppContextIO

  @Lenses
  final case class State(episode: Option[Episode] = None, hero: Pot[String] = Empty)

  class Backend($ : BackendScope[Props, State]) {

    private def queryDoc(episode: Episode) = s"""
      {
        hero(episode: $episode) {
          name
        }
      }"""

    def query(episode: Episode)(implicit ctx: AppContextIO): IO[Unit] =
      for {
        _    <- $.setStateIn[IO](State(episode.some, Pending()))
        json <- ctx.clients.starWars.query[Json, Json](queryDoc(episode), Json.obj())
        _ <- $.modStateIn[IO](
          State.hero.set(
            Ready(json.hcursor.downField("hero").downField("name").as[String].getOrElse("???"))
          )
        )
      } yield ()

    // val onChange: (ReactEvent, Dropdown.DropdownProps) => Callback =
    // (_, p) => query(Episode.fromString(p.value.asInstanceOf[String])))

    def onClickItem(
      implicit ctx: AppContextIO
    ): (ReactEvent, DropdownItem.DropdownItemProps) => Callback =
      (_, p) => query(Episode.fromString(p.value.asInstanceOf[String])).runInCB

    def render(props: Props, state: State) =
      <.div(
        Dropdown(
          placeholder = "Select episode...",
          selection   = true,
          value       = state.episode.map(_.toString).orUndefined,
          options = Episode.all.map { e =>
            DropdownItem(
              text     = e.toString,
              value    = e.toString,
              onClickE = onClickItem(props)
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
      .builder[Props]("EpisodeHero")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply()(implicit props: Props) = component(props)
}
