// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.model.ObsAttachment
import explore.model.enums.FileExtension
import explore.model.enums.ObsAttachmentType
import io.circe.*
import io.circe.generic.semiauto
import io.circe.generic.semiauto.*
import io.circe.parser.*
import io.circe.refined.given
import lucuma.core.util.Enumerated

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
@JSGlobalScope
private object Globals extends js.Object:
  val enumMetadataString: String = js.native // Defined in main.jsx

object DynamicEnums:
  val parsedEnums: ACursor =
    parse(Globals.enumMetadataString) match
      case Left(err)   => err.printStackTrace; throw err
      case Right(json) => json.hcursor

  // The givens are apparently (probably) constructed lazily.
  // See https://alexn.org/blog/2022/05/11/implicit-vs-scala-3-given/
  // We want to fail immediately if there is a problem, so we'll reference
  // the enumerated givens here. Add any new enums to this list.
  Enumerated[ObsAttachmentType]

  given Enumerated[ObsAttachmentType] =
    given Decoder[FileExtension]     = Decoder.instance: c =>
      c.downField("fileExtension").as[String].map(FileExtension(_))
    // This is a meta decoder, not a decoder for enum instances (which comes from the `Enumerated` instance)
    given Decoder[ObsAttachmentType] =
      semiauto.deriveDecoder

    val values =
      parsedEnums.downField("obsAttachmentTypeMeta").as[List[ObsAttachmentType]] match
        case Left(err)   => err.printStackTrace; throw err
        case Right(json) => json

    Enumerated.from(values.head, values.tail: _*).withTag(_.tag)

  given Decoder[ObsAttachment] = deriveDecoder
