// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.annotation.unused

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.View
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.formats._
import explore.model.reusability._
import explore.target.TargetQueries
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.units._
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.optics.syntax.all._
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.Label
import react.semanticui.sizes.Small

final case class SearchCallback(
  searchTerm: NonEmptyString,
  onComplete: Option[Target] => Callback,
  onError:    Throwable => Callback
) {
  def run: Callback = Callback.empty
}

final case class TargetBody(
  id:      Target.Id,
  target:  View[TargetResult],
  options: View[TargetVisualOptions]
) extends ReactProps[TargetBody](TargetBody.component) {
  val baseCoordinates: Coordinates =
    target.zoom(TargetQueries.baseCoordinates).get
}

object TargetBody {

  type Props = TargetBody
  val AladinRef = AladinCell.component

  implicit val propsReuse = Reusability.derive[Props]

  class Backend($ : BackendScope[Props, Unit]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinRef)

    def setName(name: String): Callback =
      $.props >>= (_.target.zoom(TargetResult.name).set(name).runInCB)

    @unused
    private def coordinatesKey(target: TargetResult): String =
      s"${target.name}#${target.tracking.baseCoordinates.show}"

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get
        .flatMapCB(_.backend.gotoRaDec(coords))
        .toCallback

    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        val target = props.target.get

        UndoRegion[TargetResult] { undoCtx =>
          val undoSet =
            UndoSet(props.id, props.target, undoCtx.setter)

          val modify = undoSet[
            (String, SiderealTracking, List[Magnitude])
          ](
            targetPropsL,
            { case (n, t, _ /*ms*/ ) =>
              input =>
                val update =
                  for {
                    _ <- EditSiderealInput.name := n.some
                    _ <- TargetQueries.updateSiderealTracking(t)
                    // _ <- EditSiderealInput.magnitudes := ms.map(m =>
                    //        MagnitudeInput(m.value.toBigDecimal, m.band, none, m.system.some)
                    //      )
                  } yield ()
                update.runS(input).value
            }
          ) _

          val modifyName = undoSet[NonEmptyString](
            unsafeTargetName,
            n => EditSiderealInput.name.set(n.value.some)
          ) _

          val modifyEpoch = undoSet[Epoch](
            TargetQueries.epoch,
            e => EditSiderealInput.epoch.set(Epoch.fromString.reverseGet(e).some)
          ) _

          val modifyProperVelocitRA = undoSet[Option[ProperVelocity.RA]](
            TargetQueries.pvRALens,
            p => {
              val pvi = ProperVelocityInput(
                ra = ProperVelocityRaInput(microarcsecondsPerYear = p.map(_.μasy.value)),
                dec = ProperVelocityDecInput(microarcsecondsPerYear =
                  TargetQueries.pvDecLens.get(target).map(_.μasy.value)
                )
              ).some
              EditSiderealInput.properVelocity.set(pvi)
            }
          ) _

          val modifyProperVelocityDec = undoSet[Option[ProperVelocity.Dec]](
            TargetQueries.pvDecLens,
            p => {
              val pvi = ProperVelocityInput(
                ra = ProperVelocityRaInput(microarcsecondsPerYear =
                  TargetQueries.pvRALens.get(target).map(_.μasy.value)
                ),
                dec = ProperVelocityDecInput(microarcsecondsPerYear = p.map(_.μasy.value))
              ).some
              EditSiderealInput.properVelocity.set(pvi)
            }
          ) _

          val modifyParallax = undoSet[Option[Parallax]](
            TargetQueries.pxLens,
            p => {
              val pxi = ParallaxModelInput(microarcseconds = p.map(_.μas.value)).some
              EditSiderealInput.parallax.set(pxi)
            }
          ) _

          val modifyRadialVelocity = undoSet[Option[RadialVelocity]](
            TargetQueries.rvLens,
            p => {
              val rvi = RadialVelocityInput(metersPerSecond =
                p.map(_.rv.withUnit[CentimetersPerSecond].value.value)
              ).some
              EditSiderealInput.radialVelocity.set(rvi)
            }
          ) _

          val searchAndSet: SearchCallback => Callback = s =>
            SimbadSearch
              .search(s.searchTerm)
              .attempt
              .runInCBAndThen {
                case Right(r @ Some(Target(n, Right(st), m))) =>
                  modify((n, st, m.values.toList)).runInCB *>
                    gotoRaDec(st.baseCoordinates) *> s.onComplete(r)
                case Right(Some(r))                           => Callback.log(s"Unknown target type $r") *> s.onComplete(none)
                case Right(None)                              => s.onComplete(none)
                case Left(t)                                  => s.onError(t)
              }

          React.Fragment(
            <.div(
              ExploreStyles.TargetGrid,
              <.div(
                CoordinatesForm(target, searchAndSet, gotoRaDec, modifyName(_).runInCB)
                  .withKey(coordinatesKey(target)),
                Form(size = Small)(
                  ExploreStyles.Grid,
                  ExploreStyles.Compact,
                  ExploreStyles.TargetPropertiesForm,
                  Label(content = s"Proper Motion",
                        basic = true,
                        clazz = ExploreStyles.FormSectionLabel |+| ExploreStyles.ColumnSpan(2)
                  ),
                  InputWithUnits(
                    props.target.zoom(TargetQueries.pvRALens),
                    ValidFormatInput.fromFormatOptional(pvRAFormat, "Must be a number"),
                    id = "raPM",
                    label = "µ RA",
                    units = "mas/y",
                    onBlur = (p: Option[ProperVelocity.RA]) => modifyProperVelocitRA(p).runInCB
                  ),
                  InputWithUnits(
                    props.target.zoom(TargetQueries.pvDecLens),
                    ValidFormatInput.fromFormatOptional(pvDecFormat, "Must be a number"),
                    id = "raDec",
                    label = "µ Dec",
                    units = "mas/y",
                    onBlur = (p: Option[ProperVelocity.Dec]) => modifyProperVelocityDec(p).runInCB
                  ),
                  InputWithUnits(
                    props.target.zoom(TargetQueries.epoch),
                    ValidFormatInput.fromFormat(Epoch.fromStringNoScheme, "Must be a number"),
                    id = "epoch",
                    label = "Epoch",
                    units = "years",
                    onBlur = (e: Epoch) => modifyEpoch(e).runInCB
                  ),
                  InputWithUnits[cats.effect.IO, Option[Parallax]](
                    props.target.zoom(TargetQueries.pxLens),
                    ValidFormatInput.fromFormatOptional(pxFormat, "Must be a number"),
                    id = "parallax",
                    label = "Parallax",
                    units = "mas",
                    onBlur = (px: Option[Parallax]) => modifyParallax(px).runInCB
                  ),
                  RVInput(props.target.zoom(TargetQueries.rvLens), modifyRadialVelocity)
                ),
                MagnitudeForm(target.magnitudes),
                UndoButtons(target, undoCtx)
              ),
              AladinRef
                .withRef(aladinRef) {
                  AladinCell(
                    props.target.zoom(TargetQueries.baseCoordinates),
                    props.options
                  )
                },
              CataloguesForm(props.options).when(false)
            ),
            <.div(
              ExploreStyles.TargetSkyplotCell,
              WIP(
                SkyPlotSection(props.baseCoordinates)
              ).when(false)
            )
          )
        }
      }

    def newProps(currentProps: Props, nextProps: Props): Callback =
      gotoRaDec(nextProps.baseCoordinates)
        .when(nextProps.baseCoordinates =!= currentProps.baseCoordinates)
        .void
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .configure(Reusability.shouldComponentUpdate)
      .build

}
