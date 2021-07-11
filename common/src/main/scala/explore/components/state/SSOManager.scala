// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import explore.model.UserVault
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import monocle.macros.Lenses
import org.typelevel.log4cats.Logger
import react.common.ReactProps

import java.time.Instant

final case class SSOManager(
  expiration:       Instant,
  setVault:         Option[UserVault] ==> SyncIO[Unit],
  setMessage:       NonEmptyString ==> SyncIO[Unit]
)(implicit val ctx: AppContextIO)
    extends ReactProps[SSOManager](SSOManager.component)

object SSOManager {
  type Props = SSOManager

  @Lenses
  case class State(cancelToken: Option[IO[Unit]])

  protected implicit val propsReuse: Reusability[Props] = Reusability.by(_.expiration)
  protected implicit val stateReuse: Reusability[State] = Reusability.never

  final class Backend() {

    def tokenRefresher(
      expiration:   Instant,
      setVault:     Option[UserVault] => SyncIO[Unit],
      setMessage:   NonEmptyString => SyncIO[Unit]
    )(implicit ctx: AppContextIO): IO[Unit] =
      for {
        vaultOpt <- ctx.sso.refreshToken(expiration)
        _        <- setVault(vaultOpt).to[IO]
        _        <- vaultOpt.fold(setMessage("Your session has expired").to[IO])(vault =>
                      tokenRefresher(vault.expiration, setVault, setMessage)
                    )
      } yield ()

    // This is a "phantom" component. Doesn't render anything.
    def render(): VdomNode = React.Fragment()
  }

  val component = ScalaComponent
    .builder[Props]
    .initialState(State(none))
    .renderBackend[Backend]
    .componentDidMount { $ =>
      implicit val ctx = $.props.ctx
      $.backend
        .tokenRefresher($.props.expiration, $.props.setVault, $.props.setMessage)
        .onError(t =>
          Logger[IO].error(t)("Error refreshing SSO token") >>
            ($.props.setVault(none) >>
              $.props.setMessage("There was an error while checking the validity of your session"))
              .to[IO]
        )
        .start
        .flatMap(ct => $.modStateIn[IO](State.cancelToken.replace(ct.cancel.some)))
        .runAsyncCB
    }
    .componentWillUnmount { $ =>
      implicit val ctx = $.props.ctx
      // Setting vault to none is defensive. This component should actually unmount when vault is none.
      $.state.cancelToken
        .map(cancel => (cancel >> $.props.setVault(none).to[IO]))
        .orEmpty
        .runAsyncCB
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
