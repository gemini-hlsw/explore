// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import lucuma.ui.forms._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.Form.FormProps
import react.semanticui.collections.form._
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class CoordinatesForm(
  tracking:        View[SiderealTracking],
  searching:       Boolean,
  goToAndSetRaDec: Coordinates => Callback
) extends ReactProps[CoordinatesForm](CoordinatesForm.component)

object CoordinatesForm {
  type Props = CoordinatesForm

  @Lenses
  final case class State(
    ra:         RightAscension,
    dec:        Declination,
    raStr:      String, // Shadow tracking of input for the case of submit without blur (enter key)
    decStr:     String, // another shadow
    raValid:    Boolean,
    decValid:   Boolean,
    raInitial:  RightAscension,
    decInitial: Declination
  )

  implicit val stateReuse                     = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] =
    Reusability.by(x => (x.tracking, x.searching))

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        val stateView = ViewF.fromState[IO]($)

        def submitRaDecForm: Form.OnSubmitE =
          (
            e: Form.ReactFormEvent,
            _: FormProps
          ) => {
            val cbo = for {
              ra  <- RightAscension.fromStringHMS.getOption(state.raStr)
              dec <- Declination.fromStringSignedDMS.getOption(state.decStr)
            } yield props.goToAndSetRaDec(Coordinates(ra, dec))

            e.preventDefaultCB *> cbo.getOrElse(Callback.empty)
          }

        Form(size = Small, onSubmitE = submitRaDecForm)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.CoordinatesForm,
          <.div(
            ExploreStyles.FlexContainer,
            ExploreStyles.TargetRaDecMinWidth,
            FormInputEV(
              id = "ra",
              value = stateView.zoom(State.ra),
              validFormat = ValidFormatInput.fromFormat(RightAscension.fromStringHMS),
              changeAuditor = ChangeAuditor.rightAscension,
              label = "RA",
              onTextChange = v => $.setStateL(State.raStr)(v),
              onValidChange = valid => $.setStateL(State.raValid)(valid),
              clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
              disabled = props.searching
            ),
            FormInputEV(
              id = "dec",
              value = stateView.zoom(State.dec),
              validFormat = ValidFormatInput.fromFormat(Declination.fromStringSignedDMS),
              changeAuditor = ChangeAuditor.declination,
              label = "Dec",
              onTextChange = v => $.setStateL(State.decStr)(v),
              onValidChange = valid => $.setStateL(State.decValid)(valid),
              clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
              disabled = props.searching
            ),
            FormButton( // does a form submission, so don't need a click handler
                        size = Small,
                        icon = true,
                        label = "Go To",
                        disabled = !state.raValid || !state.decValid || props.searching
            )(
              Icon("angle right"),
              ExploreStyles.HideLabel
            )
          )
        )
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        val ra  = props.tracking.get.baseCoordinates.ra
        val dec = props.tracking.get.baseCoordinates.dec
        stateOpt match {
          // need to update the state if we get new values for ra and dec.
          case Some(state) if ra === state.raInitial && dec === state.decInitial => state
          case _                                                                 =>
            State(
              ra = ra,
              dec = dec,
              raStr = RightAscension.fromStringHMS.reverseGet(ra),
              decStr = Declination.fromStringSignedDMS.reverseGet(dec),
              raValid = true,
              decValid = true,
              raInitial = ra,
              decInitial = dec
            )
        }
      }
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
