// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.data.NonEmptyList
import cats.effect.FiberIO
import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
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
  trigger:          Reuse[Button],
  onComplete:       Target ==> Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetSelectionPopup](TargetSelectionPopup.component)

object TargetSelectionPopup {
  type Props = TargetSelectionPopup

  implicit val reuseProps: Reusability[Props] = Reusability.derive[Props]

  implicit val reuseFiber: Reusability[FiberIO[Unit]] = Reusability.byRef

  //  BEGIN HOOK
  implicit val reuseTimeUnit: Reusability[TimeUnit]             = Reusability.byRef
  implicit val reuseFiniteDuration: Reusability[FiniteDuration] =
    Reusability.by(x => (x.length, x.unit))

  class TimeoutHandle(
    duration: FiniteDuration,
    timerRef: Hooks.UseRefF[CallbackTo, Option[FiberIO[Unit]]]
  ) {
    private val cleanup: IO[Unit] = timerRef.set(none).to[IO]

    val cancel: IO[Unit] =
      IO(timerRef.value) >>= (runningFiber =>
        (cleanup >> runningFiber.map(_.cancel).orEmpty).uncancelable
      )

    def onTimeout(effect: IO[Unit]): IO[Unit] = {
      val timedEffect =
        // We don't cleanup until `effect` completes, so that we can still invoke `cancel` when the effect is running.
        IO.sleep(duration) >> effect.guarantee(cleanup.uncancelable)
      cancel >>
        (timedEffect.start >>=
          (fiber => timerRef.set(fiber.some).to[IO])).uncancelable
    }
  }

  // Changing duration while component is mounted is not supported.
  val useDebouncedTimeout = CustomHook[FiniteDuration]
    .useRef(none[FiberIO[Unit]])
    .useMemoBy((_, _) => ())((duration, timerRef) => _ => new TimeoutHandle(duration, timerRef))
    .buildReturning((_, _, timeoutHandle) => timeoutHandle)
  //  END HOOK

  // protected val targetSources: List[TargetSource[IO]] =
  //   Program.Id.parse("p-2").map(p => TargetSource.Program[IO](p)).toList ++
  //     TargetSource.forAllCatalogs[IO]

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // inputValue
    .useStateSnapshotWithReuse("")
    // results
    .useStateWithReuse(SortedMap.empty[TargetSource[IO], NonEmptyList[Target]])
    // searching
    .useState(none[FiberIO[Unit]])
    // timer
    .custom(useDebouncedTimeout(700.milliseconds))
    // isOpen
    .useState(false)
    // targetSources
    .useMemoBy((props, _, _, _, _, _) => props.ctx)((_, _, _, _, _, _) =>
      (propsCtx: AppContextIO) => {
        implicit val ctx = propsCtx
        Program.Id.parse("p-2").map(p => TargetSource.Program[IO](p)).toList ++
          TargetSource.forAllCatalogs[IO]
      }
    )
    .renderWithReuse { (props, inputValue, results, searching, timer, isOpen, targetSources) =>
      implicit val ctx = props.ctx

      val cleanState = inputValue.setState("") >> results.setState(SortedMap.empty)

      def search(name: String): IO[Unit] =
        searching.value.map(_.cancel).orEmpty >>
          results.setStateAsync(SortedMap.empty) >>
          NonEmptyString
            .from(name)
            .toOption
            .map(nonEmptyName =>
              targetSources.value
                .traverse_(source =>
                  source.search(nonEmptyName) >>= (list =>
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

      React.Fragment(
        props.trigger.value(^.onClick --> isOpen.setState(true)),
        Modal(
          as = <.form,      // This lets us sumbit on enter
          actions = List(
            Button(size = Small, icon = true)(
              Icons.Close,
              "Cancel"
            )(^.tpe := "button", ^.key := "input-cancel")
          ),
          centered = false, // Works better on iOS
          open = isOpen.value,
          closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
          dimmer = Dimmer.Blurring,
          size = ModalSize.Small,
          onOpen = cleanState,
          onClose = timer.cancel.runAsync >> isOpen.setState(false) >> cleanState,
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
                      <.p(source.name),
                      targets.toList.map { target =>
                        <.span(
                          target.toString,
                          Button(onClick = isOpen.setState(false) >> props.onComplete(target))(
                            ^.tpe := "button"
                          )("Select")
                        )
                      }.toTagMod
                    )
                  )
                }.toTagMod
              )
            ).when(results.value.nonEmpty)
          )
        )(
          ^.onSubmit ==> (e => e.preventDefaultCB >> search(inputValue.value).runAsync)
        )
      )
    }
}
