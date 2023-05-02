// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.generic.semiauto
import lucuma.core.util.Display
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import monocle.Focus
import monocle.Lens

case class ObsAttachmentTypeMeta(
  tag:       ObsAttachmentType,
  shortName: String,
  longName:  String
) derives Eq

object ObsAttachmentTypeMeta {
  given Display[ObsAttachmentTypeMeta] = Display.by(_.shortName, _.longName)
  given Decoder[ObsAttachmentTypeMeta] = semiauto.deriveDecoder

  val tag: Lens[ObsAttachmentTypeMeta, ObsAttachmentType] = Focus[ObsAttachmentTypeMeta](_.tag)
  val shortName: Lens[ObsAttachmentTypeMeta, String]      = Focus[ObsAttachmentTypeMeta](_.shortName)
  val longName: Lens[ObsAttachmentTypeMeta, String]       = Focus[ObsAttachmentTypeMeta](_.longName)
}
