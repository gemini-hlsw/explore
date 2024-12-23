// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package japgolly.webapputil.boopickle

import boopickle.PickleState
import boopickle.Pickler
import boopickle.UnpickleState
import cats.syntax.eq.*
import japgolly.webapputil.binary.BinaryData
import japgolly.webapputil.general.Version
import japgolly.webapputil.general.Version.ordering.mkOrderingOps

import scala.annotation.elidable
import scala.util.control.NonFatal

/**
 * Binary codec (pickler). Differs from out-of-the-box [[Pickler]] in the following ways:
 *
 *   - decoding is pure: an error value is returned on failure
 *   - supports magic numbers at header and footer as a partial message integrity check
 *   - supports protocol versioning and evolution
 */
final case class SafePickler[A](
  header:  Option[MagicNumber],
  footer:  Option[MagicNumber],
  version: Version,
  body:    Pickler[A]
) {

  type Data = A

  import boopickle.{PickleImpl, UnpickleImpl}
  import SafePickler._

  // def i1 = "0x%08X".format(new util.Random().nextInt); def i2 = s"$i1, $i1"; def i4 = s"$i2 $i2"
  def withMagicNumbers(header: Int, footer: Int): SafePickler[A] =
    copy(Some(MagicNumber(header)), Some(MagicNumber(footer)))

  def withMagicNumberFooter(footer: Int): SafePickler[A] =
    copy(footer = Some(MagicNumber(footer)))

  def map[B](f: Pickler[A] => Pickler[B]): SafePickler[B] =
    copy(body = f(body))

  private val picklerHeader  = header.map(pickleMagicNumber(version, _))
  private val picklerFooter  = footer.map(pickleMagicNumber(version, _))
  private val picklerVersion = pickleVersion(version)

  private val picklerCombined: Pickler[A] =
    new Pickler[A] {

      override def pickle(a: A)(implicit state: PickleState): Unit = {
        picklerHeader.foreach(_.pickle(()))
        picklerVersion.pickle(version)
        body.pickle(a)
        picklerFooter.foreach(_.pickle(()))
      }

      override def unpickle(implicit state: UnpickleState): A = {
        picklerHeader.foreach(_.unpickle)
        val v = picklerVersion.unpickle
        if (v.major =!= version.major)
          throw DecodingFailure.UnsupportedMajorVer(localVer = version, actual = v)
        try {
          val a = body.unpickle
          picklerFooter.foreach(_.unpickle)
          a
        } catch {
          case e: Throwable => throw new VerAndErr(v, e)
        }
      }
    }

  def encode(a: A): BinaryData = {
    val bb = PickleImpl.intoBytes(a)(implicitly, picklerCombined)
    BinaryData.unsafeFromByteBuffer(bb)
  }

  private def wrapRead(unpickle: => A): SafePickler.Result[A] =
    try
      Right(unpickle)
    catch {
      case e: EmbeddedFailure => Left(e.failure)
      case e: VerAndErr       => DecodingFailure.fromException(version, e.err, Some(e.ver))
      case e: Throwable       => DecodingFailure.fromException(version, e, None)
    }

  def decode(bin: BinaryData): SafePickler.Result[A] =
    wrapRead(UnpickleImpl(picklerCombined).fromBytes(bin.unsafeByteBuffer))

  def decodeOrThrow(bin: BinaryData): A =
    decode(bin).fold(throw _, identity)

  val decodeBytes: Array[Byte] => SafePickler.Result[A] =
    bytes => decode(BinaryData.unsafeFromArray(bytes))

  def embeddedWrite(a: A)(implicit state: PickleState): Unit =
    picklerCombined.pickle(a)

  def embeddedRead(implicit state: UnpickleState): A =
    wrapRead(picklerCombined.unpickle) match {
      case Right(a) => a
      case Left(e)  => throw new EmbeddedFailure(e)
    }
}

object SafePickler {

  type Result[+A] = Either[DecodingFailure, A]

  def success[A](a: A): Result[A] =
    Right(a)

  sealed trait DecodingFailure extends RuntimeException {
    val localVer: Version
    val upstreamVer: Option[Version]

    def isLocalKnownToBeOutOfDate: Boolean =
      upstreamVer.exists(localVer < _)

    def isUpstreamKnownToBeOutOfDate: Boolean =
      upstreamVer.exists(_ < localVer)
  }

  object DecodingFailure {

    final case class UnsupportedMajorVer(localVer: Version, actual: Version)
        extends RuntimeException
        with DecodingFailure {
      override val upstreamVer = Some(actual)
    }

    final case class MagicNumberMismatch(
      localVer:    Version,
      actual:      MagicNumber,
      expected:    MagicNumber,
      upstreamVer: Option[Version]
    ) extends RuntimeException
        with DecodingFailure

    final case class InvalidVersion(localVer: Version, major: Int, minor: Int)
        extends RuntimeException
        with DecodingFailure {
      override val upstreamVer = None
    }

    final case class ExceptionOccurred(
      localVer:    Version,
      exception:   Throwable,
      upstreamVer: Option[Version]
    ) extends RuntimeException
        with DecodingFailure {

      @elidable(elidable.ASSERTION)
      private def devOnly(): Unit =
        exception.printStackTrace(System.err)

      devOnly()
    }

    def fromException(
      localVer:    Version,
      err:         Throwable,
      upstreamVer: Option[Version]
    ): Result[Nothing] =
      err match {
        case MagicNumberMismatch(_, a, b, None) =>
          Left(MagicNumberMismatch(localVer, a, b, upstreamVer))
        case e: DecodingFailure                 => Left(e)
        case e: EmbeddedFailure                 => Left(e.failure)
        case e: StackOverflowError              =>
          Left(DecodingFailure.ExceptionOccurred(localVer, e, upstreamVer))
        case NonFatal(e)                        => Left(DecodingFailure.ExceptionOccurred(localVer, e, upstreamVer))
        case e                                  => throw e
      }
  }

  private[SafePickler] final class VerAndErr(val ver: Version, val err: Throwable)
      extends RuntimeException

  private[SafePickler] final class EmbeddedFailure(val failure: DecodingFailure)
      extends RuntimeException

  private[SafePickler] def pickleVersion(localVer: Version): Pickler[Version] =
    new Pickler[Version] {
      override def pickle(a: Version)(implicit state: PickleState): Unit = {
        state.enc.writeInt(a.major.value)
        state.enc.writeInt(a.minor.value)
        ()
      }

      override def unpickle(implicit state: UnpickleState): Version = {
        val major = state.dec.readInt
        val minor = state.dec.readInt
        val minOk = major >= 1 && minor >= 0
        val maxOk = major <= 4 && minor <= 100
        if (minOk && maxOk)
          Version.fromInts(major, minor)
        else
          throw DecodingFailure.InvalidVersion(localVer, major, minor)
      }
    }

  private[SafePickler] def pickleMagicNumber(localVer: Version, real: MagicNumber): Pickler[Unit] =
    new Pickler[Unit] {
      override def pickle(a: Unit)(implicit state: PickleState): Unit =
        state.enc.writeRawInt(real.value)
        ()

      override def unpickle(implicit state: UnpickleState): Unit = {
        val found = state.dec.readRawInt
        if (found != real.value)
          throw DecodingFailure.MagicNumberMismatch(localVer = localVer,
                                                    actual = MagicNumber(found),
                                                    expected = real,
                                                    upstreamVer = None
          )
      }
    }

  object ConstructionHelperImplicits {
    implicit class SafePickler_PicklerExt[A](private val self: Pickler[A]) extends AnyVal {
      @inline def asVersion(major: Int, minor: Int): SafePickler[A]   = asVersion(
        Version.fromInts(major, minor)
      )
      def asVersion(v:   Version): SafePickler[A]                     = SafePickler(None, None, v, self)
      def asV1(minorVer: Int): SafePickler[A]                         = asVersion(Version.v1(minorVer))
      def asV2(minorVer: Int): SafePickler[A]                         = asVersion(Version.v2(minorVer))
    }
  }
}

final case class MagicNumber(value: Int) {
  override def toString = s"MagicNumber(0x$hex)"
  def hex               = "%08X".format(value)
}
