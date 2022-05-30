// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for guide probe
 * @group Enumerations (Generated)
 */
sealed abstract class GuideProbe(val tag: String) extends Product with Serializable

object GuideProbe {

  /** @group Constructors */
  case object AOWFS extends GuideProbe("AOWFS")

  /** @group Constructors */
  case object OIWFS extends GuideProbe("OIWFS")

  /** @group Constructors */
  case object PWFS extends GuideProbe("PWFS")

  /** All members of GuideProbe, in canonical order. */
  val all: List[GuideProbe] =
    List(AOWFS, OIWFS, PWFS)

  /** Select the member of GuideProbe with the given tag, if any. */
  def fromTag(s: String): Option[GuideProbe] =
    all.find(_.tag === s)

  /** Select the member of GuideProbe with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GuideProbe =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GuideProbe: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GuideProbeEnumerated: Enumerated[GuideProbe] =
    Enumerated.of[GuideProbe](AOWFS, OIWFS, PWFS)

}
