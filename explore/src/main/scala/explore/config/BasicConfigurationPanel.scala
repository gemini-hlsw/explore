// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.ImagingConfigurationOptions
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.Spectroscopy
import explore.model.itc.ItcTarget
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceMode
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

case class BasicConfigurationPanel(
  userId:           Option[User.Id],
  obsId:            Observation.Id,
  spectroscopyView: ViewOpt[Spectroscopy],
  selectedConfig:   View[Option[BasicConfigAndItc]],
  constraints:      ConstraintSet,
  itcTargets:       List[ItcTarget],
  baseCoordinates:  Option[CoordinatesAtVizTime],
  createConfig:     IO[Unit],
  confMatrix:       SpectroscopyModesMatrix,
  readonly:         Boolean
) extends ReactFnProps(BasicConfigurationPanel.component)

private object BasicConfigurationPanel:
  private type Props = BasicConfigurationPanel

  private object Creating extends NewType[Boolean]

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[ScienceMode](ScienceMode.Spectroscopy)
      .useStateView[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .useStateView(Creating(false))
      .render { (props, ctx, mode, imaging, creating) =>
        import ctx.given

        val canAccept: Boolean =
          props.selectedConfig.get.flatMap(_.itcResult).flatMap(_.toOption).exists(_.isSuccess)

        // wavelength has to be handled special because you can't select a row without a wavelength.
        val message: String =
          props.spectroscopyView.get
            .map(_.wavelength)
            .fold("Wavelength is required for creating a configuration.")(_ =>
              props.selectedConfig.get match {
                case Some(BasicConfigAndItc(_, itc)) =>
                  itc match {
                    case Some(Right(r)) if r.isPending => "Waiting for ITC result..."
                    case Some(Right(r)) if r.isSuccess =>
                      "Click `Accept` to create configuration and view details."
                    case _                             => "ITC issues must be fixed before creating a configuration."
                  }

                case None => "To create a configuration, select a table row."
              }
            )

        val buttonIcon: FontAwesomeIcon =
          if (creating.get.value) Icons.Spinner.withSpin(true)
          else Icons.Gears

        <.div(ExploreStyles.BasicConfigurationGrid)(
          <.div(
            ExploreStyles.BasicConfigurationForm,
            // TODO Enable when imaging is available
            // <.label("Mode", HelpIcon("configuration/mode.md".refined)),
            // FormEnumDropdownView(id = "configuration-mode".refined,
            //                      value = mode,
            //                      disabled = props.readonly
            // ),
            props.spectroscopyView.mapValue(v => SpectroscopyConfigurationPanel(v, props.readonly))
            // TODO Pending reinstate
            // ImagingConfigurationPanel(imaging)
            //   .unless(isSpectroscopy)
          ),
          props.spectroscopyView.mapValue(spectroscopy =>
            SpectroscopyModesTable(
              props.userId,
              props.selectedConfig,
              spectroscopy.get,
              props.constraints,
              if (props.itcTargets.isEmpty) none else props.itcTargets.some,
              props.baseCoordinates,
              props.confMatrix
            )
          ),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            Message(text = message),
            Button(
              "Accept Configuration",
              icon = buttonIcon,
              disabled = creating.get.value || !canAccept,
              severity = Button.Severity.Secondary,
              onClick = props.createConfig.switching(creating.async, Creating(_)).runAsync
            ).compact.small.when(canAccept)
          ).when(props.spectroscopyView.get.isDefined && !props.readonly)
        )
      }
