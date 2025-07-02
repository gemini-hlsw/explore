// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class AdvancedConfigButtons(
  editState:            View[ConfigEditState],
  isCustomized:         Boolean,
  revertConfig:         Callback,
  revertCustomizations: Callback,
  sequenceChanged:      Callback,
  readonly:             Boolean,
  showAdvancedButton:   Boolean = true,
  showCustomizeButton:  Boolean = true // The "Advanced Customization" button
) extends ReactFnProps(AdvancedConfigButtons)

object AdvancedConfigButtons
    extends ReactFnComponent[AdvancedConfigButtons](props =>
      if (props.readonly) EmptyVdom
      else
        <.div(
          ExploreStyles.AdvancedConfigurationButtons,
          Button(
            label = "Revert Configuration",
            icon = Icons.ListIcon,
            severity = Button.Severity.Secondary,
            onClick = props.revertConfig
          ).compact.small
            .unless(props.isCustomized),
          Button(
            label = "Revert Customizations",
            icon = Icons.TrashUnstyled,
            severity = Button.Severity.Danger,
            onClick = props.sequenceChanged *> props.editState.set(ConfigEditState.View) >>
              props.revertCustomizations
          ).compact.small
            .when(props.isCustomized),
          Button(
            label = "Customize",
            icon = Icons.Edit,
            severity = Button.Severity.Secondary,
            onClick = props.editState.set(ConfigEditState.SimpleEdit)
          ).compact.small
            .when(props.editState.get === ConfigEditState.View && props.showCustomizeButton),
          Button(
            label = "Advanced Customization",
            icon = Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
            severity = Button.Severity.Secondary,
            onClick = props.editState.set(ConfigEditState.AdvancedEdit)
          ).compact.small
            .when(props.editState.get === ConfigEditState.SimpleEdit && props.showAdvancedButton)
        )
    )

// Use this only for testing for new modes
// gives you an escape hatch to revert the config
case class RollbackOnlyConfigButtons(revertConfig: Callback)
    extends ReactFnProps(RollbackOnlyConfigButtons)

object RollbackOnlyConfigButtons
    extends ReactFnComponent[RollbackOnlyConfigButtons](props =>
      useStateView(ConfigEditState.View).map: editState =>
        AdvancedConfigButtons(
          editState = editState,
          isCustomized = false,
          revertConfig = props.revertConfig,
          revertCustomizations = Callback.empty,
          sequenceChanged = Callback.empty,
          readonly = false,
          showAdvancedButton = false,
          showCustomizeButton = false
        )
    )
