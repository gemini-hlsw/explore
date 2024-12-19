// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.model.*
import explore.model.AppContext
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
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.scalajs.dom

import scalajs.js.JSConverters.*

case class SearchForm(
  id:         Target.Id,
  targetName: View[NonEmptyString],
  targetSet:  Target.Sidereal => Callback,
  searching:  View[Set[Target.Id]],
  readonly:   Boolean
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
      .render: (props, ctx, term, enabled, error, buttonRef, inputRef) =>
        import ctx.given

        val searchComplete: Callback = props.searching.mod(_ - props.id)

        def onKeyPress = (e: ReactKeyboardEvent) =>
          if (Option(e.key).exists(_ === dom.KeyValue.Enter) && !props.readonly)
            buttonRef.get >>= (_.map(button => Callback(button.click())).orEmpty)
          else
            Callback.empty

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
                disabled = props.readonly || props.searching.get.nonEmpty,
                icon = Icons.Search,
                loading = props.searching.get.nonEmpty,
                onClick = error.setState(none) >> props.searching.mod(_ + props.id),
                modifiers = List(^.untypedRef := buttonRef)
              ).tiny.compact,
              onSelected = targetWithId =>
                (targetWithId.target match
                  case t @ Target.Sidereal(_, _, _, _) => props.targetSet(t)
                  case _                               => Callback.empty
                ) >> searchComplete,
              onCancel = searchComplete,
              initialSearch = term.get.some,
              showCreateEmpty = false
            )
          else Icons.Ban

        val disabled = props.searching.get.exists(_ === props.id) || props.readonly

        // use a form here to handle submit? is it embedded?
        FormInputTextView(
          id = "search".refined,
          value = term.withOnMod(props.targetName.set),
          label = React.Fragment("Name", HelpIcon("target/main/search-target.md".refined)),
          validFormat = InputValidSplitEpi.nonEmptyString,
          error = error.value.orUndefined,
          disabled = disabled,
          postAddons = List(searchIcon).filter(_ => !props.readonly),
          onTextChange = (_: String) => error.setState(none),
          onValidChange = valid => enabled.setState(valid),
          placeholder = "Name"
        ).withMods(^.onKeyPress ==> onKeyPress, ^.untypedRef := inputRef)
