// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq
import cats.derived.*
import lucuma.core.util.Display
import lucuma.schemas.ObservationDB
import monocle.Focus
import monocle.Lens

// This class can go in core or schemas.
// The ObservationDB schema can map ObsAttachmentTypeMeta to this type (which will remove the need for mapping in the query)
case class ObsAttachmentType(
  tag:       String,
  shortName: String,
  longName:  String
) derives Eq

object ObsAttachmentType:
  given Display[ObsAttachmentType] = Display.by(_.shortName, _.longName)

  val tag: Lens[ObsAttachmentType, String]       = Focus[ObsAttachmentType](_.tag)
  val shortName: Lens[ObsAttachmentType, String] = Focus[ObsAttachmentType](_.shortName)
  val longName: Lens[ObsAttachmentType, String]  = Focus[ObsAttachmentType](_.longName)

  // Decided to declare this in EnumQueriesGQL, it's only needed there and never again.
  // object Meta:
  //   val Decoder: Decoder[ObsAttachmentType] = semiauto.deriveDecoder
