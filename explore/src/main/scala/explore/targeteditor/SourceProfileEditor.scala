// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import clue.data.syntax.*
import eu.timepit.refined.auto.*
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.enums.SourceProfileType
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.EnumDropdown
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import queries.schemas.*
import queries.schemas.odb.ODBConversions.*
import react.common.ReactFnProps

case class SourceProfileEditor(
  sourceProfile: Aligner[SourceProfile, SourceProfileInput],
  disabled:      Boolean
) extends ReactFnProps(SourceProfileEditor.component)

object SourceProfileEditor:
  private type Props = SourceProfileEditor

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .render { (props, ctx) =>
        import ctx.given

        val gaussianAlignerOpt: Option[Aligner[Gaussian, GaussianInput]] =
          props.sourceProfile.zoomOpt(
            SourceProfile.gaussian,
            forceAssign(SourceProfileInput.gaussian.modify)(GaussianInput())
          )

        React.Fragment(
          FormLabel(htmlFor = "profile-type".refined)(
            "Profile",
            HelpIcon("target/main/target-profile.md".refined)
          ),
          EnumDropdown[SourceProfileType](
            id = "profile-type".refined,
            value = SourceProfileType.fromSourceProfile(props.sourceProfile.get),
            onChange = sp => props.sourceProfile.view(_.toInput).mod(sp.convert),
            clazz = LucumaStyles.FormField
          ),
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
                FormInputTextView(                          // FWHM is positive arcsec accepting decimals
                  id = "fwhm".refined,
                  value = gaussianAligner
                    .zoom(Gaussian.fwhm, GaussianInput.fwhm.modify)
                    .view(_.toInput.assign),
                  label = "FWHM",
                  validFormat = MathValidators.angleArcSec, // THIS IS ARCSEC AND NOT SIGNED!
                  changeAuditor = ChangeAuditor
                    .fromInputValidSplitEpi(MathValidators.angleArcSec)
                    .denyNeg
                    .allowEmpty,
                  postAddons = List("arcsec")
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
