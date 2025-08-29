// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.model.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.scalajs.dom

import scala.concurrent.duration.*

import scalajs.js.JSConverters.*

case class SearchForm(
  id:            Target.Id,
  targetName:    View[NonEmptyString],
  targetSet:     Target => Callback,
  searching:     View[Set[Target.Id]],
  readonly:      Boolean,
  cloningTarget: Boolean
) extends ReactFnProps[SearchForm](SearchForm.component)

object SearchForm:
  private type Props = SearchForm

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateViewBy((props, _) => props.targetName.get) // term
      .useState(true)                                     // enabled
      .useState(none[NonEmptyString])                     // error
      .useEffectWithDepsBy((props, _, _, _, _) => props.targetName.get)((_, _, term, enabled, _) =>
        name => term.set(name) >> enabled.setState(true)
      )
      .useRefToVdom[dom.HTMLButtonElement]
      .useRefToVdom[dom.HTMLInputElement]
      .useEffectWithDepsBy((props, _, _, _, _, _, inputRef) => (props.id, inputRef))(
        (props, _, _, _, _, _, _) =>
          // Auto select name field on new targets
          (_, inputRef) =>
            inputRef.get
              .flatMap(
                _.foldMap(i => Callback(i.select()).delayMs(50).toCallback)
              )
              .when_(props.targetName.get === NewTargetName)
      )
      .useSingleEffect
      .render: (props, ctx, term, enabled, error, buttonRef, inputRef, singleEffect) =>
        import ctx.given

        val searchComplete: Callback = props.searching.mod(_ - props.id)

        def onKeyPress = (e: ReactKeyboardEvent) =>
          if (Option(e.key).exists(_ === dom.KeyValue.Enter) && !props.readonly)
            // we just click the button, which deals with the name update/cloning/delay
            // as details in comments below.
            buttonRef.get >>= (_.map(button => Callback(button.click())).orEmpty)
          else
            Callback.empty

        def onButtonClick: Callback =
          // This will cancel the name update, as described in a comment below.
          singleEffect
            .submit(
              (error.setState(none) >> props.searching.mod(_ + props.id)).toAsync
            )
            .runAsync

        // if the user cancels the search, the term (in the text input) might not
        // match the target name, so we need to update the target name to match.
        // This is the behavior requested by Andy.
        def updateNameIfNeeded: Callback =
          if (term.get =!= props.targetName.get)
            props.targetName.set(term.get)
          else Callback.empty

        val searchIcon: VdomNode =
          if (enabled.value && !props.readonly)
            TargetSelectionPopup(
              "Replace Target Data",
              TargetSource.forAllSiderealCatalogs[IO],
              "",
              Icons.Ban,
              "Fetch",
              Icons.ArrowDownLeft,
              trigger = Button(
                severity = Button.Severity.Success,
                disabled = props.cloningTarget || props.searching.get.nonEmpty,
                icon = Icons.Search,
                loading = props.searching.get.nonEmpty,
                onClick = onButtonClick,
                modifiers = List(^.untypedRef := buttonRef)
              ).tiny.compact,
              onSelected = targetWithId => props.targetSet(targetWithId.target) >> searchComplete,
              onCancel = updateNameIfNeeded >> searchComplete,
              initialSearch = term.get.some,
              showCreateEmpty = false
            )
          else Icons.Ban

        val disabled =
          props.searching.get.exists(_ === props.id) || props.readonly || props.cloningTarget

        FormInputTextView(
          id = "search".refined,
          value = term.withOnMod(nes =>
            // We submit to the singleEffect with a delay. If the user hit Enter or they
            // click on the button before leaving the input field, this will get cancelled
            // so that the name doesn't get updated and, more importantly, no target cloning
            // occurs, which messed everything up.
            singleEffect
              .submit(IO.sleep(200.milliseconds) >> props.targetName.set(nes).toAsync)
              .runAsync
          ),
          label = React.Fragment("Name", HelpIcon("target/main/search-target.md".refined)),
          validFormat = InputValidSplitEpi.nonEmptyString,
          error = error.value.orUndefined,
          disabled = disabled,
          postAddons = List(searchIcon).filter(_ => !props.readonly),
          onTextChange = (_: String) => error.setState(none),
          onValidChange = valid => enabled.setState(valid),
          placeholder = "Name"
        ).withMods(^.onKeyPress ==> onKeyPress, ^.untypedRef := inputRef)
