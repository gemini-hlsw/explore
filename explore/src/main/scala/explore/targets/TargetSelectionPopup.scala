package explore.targets

import cats.Order._
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.SimbadSearch
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.util.Enumerated
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import org.typelevel.log4cats.Logger
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import java.util.concurrent.TimeUnit
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

final case class TargetSelectionPopup(
  trigger:          Reuse[VdomNode],
  onComplete:       SiderealTarget ==> Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetSelectionPopup](TargetSelectionPopup.component)

object TargetSelectionPopup {
  type Props = TargetSelectionPopup

  implicit val reuseProps: Reusability[Props] = Reusability.derive[Props]

  protected sealed trait TargetSource extends Product with Serializable {
    def search[F[_]: Async: Logger](name: NonEmptyString): F[List[SiderealTarget]]
  }
  protected object TargetSource {
    case object Program extends TargetSource {
      override def search[F[_]: Async: Logger](name: NonEmptyString): F[List[SiderealTarget]] =
        Sync[F].delay(List.empty)
    }
    case object Simbad  extends TargetSource {
      override def search[F[_]: Async: Logger](name: NonEmptyString): F[List[SiderealTarget]] =
        // IO.println(s"Searching for $name in Simbad") >>
        SimbadSearch
          .search[F](name)
          .map(_.toList)
    }

    implicit val enumTargetSource: Enumerated[TargetSource] = Enumerated.of(Program, Simbad)
  }

  implicit val reuseFiber: Reusability[FiberIO[Unit]] = Reusability.byRef

  //  BEGIN HOOK
  implicit val reuseTimeUnit: Reusability[TimeUnit]             = Reusability.byRef
  implicit val reuseFiniteDuration: Reusability[FiniteDuration] =
    Reusability.by(x => (x.length, x.unit))

  // Adapted from https://stackoverflow.com/a/59274757
  class TimeoutHandle(
    duration: FiniteDuration,
    timerRef: Hooks.UseRefF[CallbackTo, Option[FiberIO[Unit]]]
  ) {
    private val cleanup: IO[Unit] = timerRef.set(none).to[IO]

    val cancel: IO[Unit] = {
      val runningFiber = timerRef.value
      (cleanup >> runningFiber.map(_.cancel).orEmpty).uncancelable
    }

    def onTimeout(effect: IO[Unit]): IO[Unit] = {
      val timedEffect =
        // We cleanup after `effect` completes, so that we can still invoke `cancel` when the effect is running.
        IO.sleep(duration) >> effect.guarantee(cleanup.uncancelable)
      cancel >>
        (timedEffect.start >>=
          (fiber => timerRef.set(fiber.some).to[IO])).uncancelable
    }
  }

  val useResetTimer = CustomHook[FiniteDuration]
    .useRef(none[FiberIO[Unit]])
    .buildReturning((duration, timerRef) =>
      Reusable
        .implicitly(duration)
        .withValue(
          new TimeoutHandle(duration, timerRef)
        )
    )
  //  END HOOK

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // inputValue
    .useStateSnapshotWithReuse("")
    // results
    .useStateWithReuse(SortedMap.empty[TargetSource, NonEmptyList[SiderealTarget]])
    // searching
    .useState(none[FiberIO[Unit]])
    .custom(useResetTimer(2.seconds))
    .renderWithReuse { (props, inputValue, results, searching, timer) =>
      implicit val ctx = props.ctx

      val cleanState = inputValue.setState("") >> results.setState(SortedMap.empty)

      def search(name: String): IO[Unit] =
        searching.value.map(_.cancel).orEmpty >>
          results.setStateAsync(SortedMap.empty) >>
          NonEmptyString
            .from(name)
            .toOption
            .map(nonEmptyName =>
              Enumerated[TargetSource].all
                .traverse_(source =>
                  source.search[IO](nonEmptyName) >>= (list =>
                    NonEmptyList
                      .fromList(list)
                      .map(nel => results.modStateAsync(_ + (source -> nel)): IO[Unit])
                      .orEmpty
                  )
                )
                .guarantee(
                  searching.setStateAsync(none) >> IO.println("Finished searching")
                )
                .start >>= (fiber => searching.setStateAsync(fiber.some))
            )
            .orEmpty

      Modal(
        as = <.form,      // This lets us sumbit on enter
        actions = List(
          Button(size = Small, icon = true)(
            Icons.Close,
            "Cancel"
          )(^.key := "input-cancel")
        ),
        centered = false, // Works better on iOS
        trigger = props.trigger.value,
        closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
        dimmer = Dimmer.Blurring,
        size = ModalSize.Small,
        onClose = cleanState,
        header = ModalHeader("Search Target"),
        content = ModalContent(
          FormInputEV(
            id = NonEmptyString("name"),
            value = inputValue,
            // icon = Icons.Search,
            // iconPosition = IconPosition.Left,
            onTextChange = t => inputValue.setState(t) >> timer.onTimeout(search(t)).runAsync,
            loading = searching.value.nonEmpty
          )
            .withMods(^.placeholder := "Name", ^.autoFocus := true),
          Segment(
            <.div(
              results.value.map { case (source, targets) =>
                Segment(
                  <.div(
                    <.p(Enumerated[TargetSource].tag(source)),
                    targets.toList.map { target =>
                      target.toString
                    }.toTagMod
                  )
                )
              }.toTagMod
            )
          ).when(results.value.nonEmpty)
        )
      )

    }
}
