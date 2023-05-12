// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.display.*
import io.circe.Decoder
import io.circe.JsonObject
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import monocle.Focus
import org.typelevel.cats.time.*

import java.time.Instant
import scala.collection.immutable.SortedSet
import lucuma.schemas.ObservationDB.Types.ScienceRequirementsInput
import lucuma.schemas.model.ObservingMode

// TODO Rename to Observation??
case class ObsSummary(
  id:                  Observation.Id,
  title:               String,
  subtitle:            Option[NonEmptyString],
  status:              ObsStatus,
  activeStatus:        ObsActiveStatus,
  executionTime:       TimeSpan,
  scienceTargetIds:    SortedSet[Target.Id],
  constraints:         ConstraintSet,
  timingWindows:       List[TimingWindow],
  //
  scienceRequirements: ScienceRequirements,
  observingMode:       Option[ObservingMode],
  //
  visualizationTime:   Option[Instant],
  posAngleConstraint:  Option[PosAngleConstraint],
  wavelength:          Option[Wavelength]
) derives Eq:
  lazy val configurationSummary: Option[String] = observingMode.map(_.toBasicConfiguration) match
    case Some(BasicConfiguration.GmosNorthLongSlit(grating, _, fpu, _)) =>
      s"GMOS-N ${grating.shortName} ${fpu.shortName}".some
    case Some(BasicConfiguration.GmosSouthLongSlit(grating, _, fpu, _)) =>
      s"GMOS-S ${grating.shortName} ${fpu.shortName}".some
    case _                                                              =>
      none

  lazy val constraintsSummary: String =
    s"${constraints.imageQuality.label} ${constraints.cloudExtinction.label} ${constraints.skyBackground.label} ${constraints.waterVapor.label}"

object ObsSummary:
  val id                  = Focus[ObsSummary](_.id)
  val title               = Focus[ObsSummary](_.title)
  val subtitle            = Focus[ObsSummary](_.subtitle)
  val status              = Focus[ObsSummary](_.status)
  val activeStatus        = Focus[ObsSummary](_.activeStatus)
  val scienceTargetIds    = Focus[ObsSummary](_.scienceTargetIds)
  val constraints         = Focus[ObsSummary](_.constraints)
  val timingWindows       = Focus[ObsSummary](_.timingWindows)
  val scienceRequirements = Focus[ObsSummary](_.scienceRequirements)
  val observingMode       = Focus[ObsSummary](_.observingMode)
  val visualizationTime   = Focus[ObsSummary](_.visualizationTime)
  val posAngleConstraint  = Focus[ObsSummary](_.posAngleConstraint)
  val wavelength          = Focus[ObsSummary](_.wavelength)

  private case class TargetIdWrapper(id: Target.Id)
  private object TargetIdWrapper:
    given Decoder[TargetIdWrapper] = deriveDecoder

  given Decoder[ObsSummary] = Decoder.instance(c =>
    for {
      id                  <- c.get[Observation.Id]("id")
      title               <- c.get[String]("title")
      subtitle            <- c.get[Option[NonEmptyString]]("subtitle")
      status              <- c.get[ObsStatus]("status")
      activeStatus        <- c.get[ObsActiveStatus]("activeStatus")
      executionTime       <- c.downField("plannedTime").get[TimeSpan]("execution")
      scienceTargetIds    <- c.downField("targetEnvironment").get[List[TargetIdWrapper]]("asterism")
      constraints         <- c.get[ConstraintSet]("constraintSet")
      timingWindows       <- c.get[List[TimingWindow]]("timingWindows")
      scienceRequirements <- c.get[ScienceRequirements]("scienceRequirements")
      observingMode       <- c.get[Option[ObservingMode]]("observingMode")
      visualizationTime   <- c.get[Option[Timestamp]]("visualizationTime")
      posAngleConstraint  <- c.get[Option[PosAngleConstraint]]("posAngleConstraint")
      wavelength          <- c.downField("scienceRequirements")
                               .downField("spectroscopy")
                               .get[Option[Wavelength]]("wavelength")
    } yield ObsSummary(
      id,
      title,
      subtitle,
      status,
      activeStatus,
      executionTime,
      SortedSet.from(scienceTargetIds.map(_.id)),
      constraints,
      timingWindows,
      scienceRequirements,
      observingMode,
      visualizationTime.map(_.toInstant),
      posAngleConstraint,
      wavelength
    )
  )
