// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.validation.InputValidSplitEpi
import lucuma.refined.*
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.abbreviate
import org.scalajs.dom
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.ext.KeyValue
import org.typelevel.log4cats.Logger
import react.common.ReactFnProps
import react.primereact.Button

import scalajs.js.timers
import scalajs.js.JSConverters.*

case class SearchForm(
  id:         Target.Id,
  targetName: View[NonEmptyString],
  targetSet:  Target.Sidereal => Callback,
  searching:  View[Set[Target.Id]]
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
      .useEffectWithDepsBy((props, _, _, _, _) => props.id)((props, _, _, _, _) =>
        _ => // Auto-select name field on new targets.
          // Accessing dom elements by id is not ideal in React, but passing a ref to the <input> element of a
          // Form.Input is a bit convoluted. We should reevaluate when and if we switch to another component library.
          // TODO REEVALUATE NOW!
          CallbackTo(
            Option(dom.document.getElementById("search"))
              .foldMap(node =>
                Callback(timers.setTimeout(0)(node.asInstanceOf[dom.HTMLInputElement].select()))
              )
          ).flatten
            .when(props.targetName.get === NewTargetName)
            .void
      )
      .useRefToVdom[HTMLButtonElement]
      .render { (props, ctx, term, enabled, error, buttonRef) =>
        import ctx.given

        val searchComplete: Callback = props.searching.mod(_ - props.id)

        def onKeyPress = (e: ReactKeyboardEvent) =>
          if (Option(e.key).exists(_ === KeyValue.Enter))
            buttonRef.get >>= (_.map(button => Callback(button.click())).orEmpty)
          else
            Callback.empty

        val searchIcon: VdomNode =
          if (enabled.value)
            TargetSelectionPopup(
              "Replace Target Data",
              TargetSource.forAllSiderealCatalogs[IO],
              "",
              Icons.Ban,
              "Fetch",
              Icons.ArrowDownLeft,
              trigger = Button(
                severity = Button.Severity.Success,
                disabled = props.searching.get.nonEmpty,
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

        val disabled = props.searching.get.exists(_ === props.id)

        // use a form here to handle submit? is it embedded?
        FormInputTextView(
          id = "search".refined,
          value = term.withOnMod(props.targetName.set),
          label = React.Fragment("Name", HelpIcon("target/main/search-target.md".refined)),
          validFormat = InputValidSplitEpi.nonEmptyString,
          error = error.value.orUndefined,
          disabled = disabled,
          postAddons = List(searchIcon),
          onTextChange = (_: String) => error.setState(none),
          onValidChange = valid => enabled.setState(valid),
          placeholder = "Name"
        ).withMods(^.onKeyPress ==> onKeyPress)
      }
