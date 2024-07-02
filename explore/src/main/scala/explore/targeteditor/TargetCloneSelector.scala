// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.ObsIdSet
import explore.model.TargetEditCloneInfo
import explore.model.TargetEditObsInfo
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.BooleanRadioButtons
import lucuma.ui.primereact.given

final case class TargetCloneSelector(
  obsInfo:    TargetEditObsInfo,
  toCloneFor: View[Option[ObsIdSet]]
) extends ReactFnProps(TargetCloneSelector.component)

object TargetCloneSelector:
  private type Props = TargetCloneSelector

  private object EditScope extends NewType[Boolean]:
    inline def AllInstances: EditScope = EditScope(true)
    inline def CurrentOnly: EditScope  = EditScope(false)

  private type EditScope = EditScope.Type

  private given Reusability[EditScope] = Reusability.byEq

  extension (cloneInfo: TargetEditCloneInfo)
    private def cloneForScope(scope: EditScope): Option[ObsIdSet] =
      if (scope === EditScope.CurrentOnly) cloneInfo.cloneForCurrent else cloneInfo.cloneForAll

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(props => props.obsInfo)(_ => TargetEditCloneInfo.fromObsInfo)
      .useStateView(EditScope.CurrentOnly)
      .useEffectWithDepsBy((_, info, scope) => (info, scope.get)) {
        (props, _, _) => (info, scope) =>
          props.toCloneFor.set(info.cloneForScope(scope))
      }
      .render: (props, info, editScope) =>
        if (info.noMessages) <.div()
        else
          <.div(
            ExploreStyles.SharedEditWarning,
            info.message.map(_.value),
            info.choice
              .map((currentText, allText) =>
                BooleanRadioButtons(
                  view = editScope.as(EditScope.value),
                  idBase = "editscope".refined,
                  name = "editscope".refined,
                  trueLabel = allText,
                  falseLabel = currentText
                ).toFalseTrueFragment
              )
              .getOrElse(TagMod.empty)
          )
