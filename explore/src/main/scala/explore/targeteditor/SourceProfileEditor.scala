// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import explore.model.validators._
import cats.syntax.all._
import clue.data.syntax._
import eu.timepit.refined.auto._
import explore.common._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.enum.SourceProfileType
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile._
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.reusability._
import queries.schemas.implicits._
import react.common._

case class SourceProfileEditor(
  sourceProfile:       Aligner[SourceProfile, SourceProfileInput],
  disabled:            Boolean
)(implicit val appCtx: AppContextIO)
    extends ReactFnProps[SourceProfileEditor](SourceProfileEditor.component)

object SourceProfileEditor {
  type Props = SourceProfileEditor

  protected val component = ScalaFnComponent[Props] { props =>
    implicit val appCtx = props.appCtx

    val gaussianAlignerOpt: Option[Aligner[Gaussian, GaussianInput]] =
      props.sourceProfile.zoomOpt(
        SourceProfile.gaussian,
        forceAssign(SourceProfileInput.gaussian.modify)(GaussianInput())
      )

    React.Fragment(
      <.label("Profile", ExploreStyles.SkipToNext),
      EnumSelect[SourceProfileType](
        value = SourceProfileType.fromSourceProfile(props.sourceProfile.get).some,
        onChange = sp => props.sourceProfile.view(_.toInput).mod(sp.convert)
      ),
      <.span,
      props.sourceProfile
        .zoomOpt(
          SourceProfile.point.andThen(Point.spectralDefinition),
          forceAssign(SourceProfileInput.point.modify)(SpectralDefinitionIntegratedInput())
        )
        .map(pointSpectralDefinitionAccess =>
          IntegratedSpectralDefinitionEditor(pointSpectralDefinitionAccess)
        ),
      props.sourceProfile
        .zoomOpt(
          SourceProfile.uniform.andThen(Uniform.spectralDefinition),
          forceAssign(SourceProfileInput.uniform.modify)(SpectralDefinitionSurfaceInput())
        )
        .map(uniformSpectralDefinitionAccess =>
          SurfaceSpectralDefinitionEditor(uniformSpectralDefinitionAccess)
        ),
      gaussianAlignerOpt
        .map(gaussianAligner =>
          React.Fragment(
            <.label("FWHM", ExploreStyles.SkipToNext),
            InputWithUnits( // FWHM is positive arcsec accepting decimals
              gaussianAligner.zoom(Gaussian.fwhm, GaussianInput.fwhm.modify).view(_.toInput.assign),
              angleValidFormat,
              ChangeAuditor.fromValidFormatInput(angleValidFormat).denyNeg.allowEmpty,
              id = "fwhm",
              units = "arcsec"
            ),
            IntegratedSpectralDefinitionEditor(
              gaussianAligner.zoom(
                Gaussian.spectralDefinition,
                forceAssign(GaussianInput.spectralDefinition.modify)(
                  SpectralDefinitionIntegratedInput()
                )
              )
            )
          )
        )
    )
  }
}
