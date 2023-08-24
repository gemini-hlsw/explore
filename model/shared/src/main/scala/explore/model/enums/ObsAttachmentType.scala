// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq
import cats.derived.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import monocle.Focus
import monocle.Lens

object FileExtension extends NewType[String]
type FileExtension = FileExtension.Type

// This class can go in core or schemas.
// The ObservationDB schema can map ObsAttachmentTypeMeta to this type (which will remove the need for mapping in the query)
case class ObsAttachmentType(
  tag:            String,
  shortName:      String,
  longName:       String,
  fileExtensions: List[FileExtension]
) derives Eq:
  def accept: String = fileExtensions.map("." + _.value).mkString(",")

object ObsAttachmentType:
  given Display[ObsAttachmentType] = Display.by(_.shortName, _.longName)

  val tag: Lens[ObsAttachmentType, String]                         = Focus[ObsAttachmentType](_.tag)
  val shortName: Lens[ObsAttachmentType, String]                   = Focus[ObsAttachmentType](_.shortName)
  val longName: Lens[ObsAttachmentType, String]                    = Focus[ObsAttachmentType](_.longName)
  val fileExtensions: Lens[ObsAttachmentType, List[FileExtension]] =
    Focus[ObsAttachmentType](_.fileExtensions)

  def Finder(using e: Enumerated[ObsAttachmentType]): ObsAttachmentType = e.unsafeFromTag("FINDER")
