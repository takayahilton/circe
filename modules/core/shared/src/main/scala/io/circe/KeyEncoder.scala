package io.circe

import cats.Contravariant
import java.io.Serializable
import java.util.UUID

/**
 * A type class that provides a conversion from a value of type `A` to a string.
 *
 * This type class will be used to create strings for JSON keys when encoding
 * `Map[A, ?]` instances as JSON.
 *
 * Note that if more than one value maps to the same string, the resulting JSON
 * object may have fewer fields than the original map.
 */
trait KeyEncoder[A] extends Serializable { self =>

  /**
   * Convert a key value to a string.
   */
  def apply(key: A): String

  /**
   * Construct an instance for type `B` from an instance for type `A`.
   */
  final def contramap[B](f: B => A): KeyEncoder[B] = key => self(f(key))
}

final object KeyEncoder {
  @inline def apply[A](implicit A: KeyEncoder[A]): KeyEncoder[A] = A

  def instance[A](f: A => String): KeyEncoder[A] = f(_)

  implicit val encodeKeyString: KeyEncoder[String] = identity _

  implicit val encodeKeySymbol: KeyEncoder[Symbol] = _.name

  implicit val encodeKeyUUID: KeyEncoder[UUID] = _.toString

  implicit val encodeKeyByte: KeyEncoder[Byte] = java.lang.Byte.toString(_)

  implicit val encodeKeyShort: KeyEncoder[Short] = java.lang.Short.toString(_)

  implicit val encodeKeyInt: KeyEncoder[Int] = java.lang.Integer.toString(_)

  implicit val encodeKeyLong: KeyEncoder[Long] = java.lang.Long.toString(_)

  implicit val keyEncoderContravariant: Contravariant[KeyEncoder] = new Contravariant[KeyEncoder] {
    final def contramap[A, B](e: KeyEncoder[A])(f: B => A): KeyEncoder[B] = e.contramap(f)
  }
}
