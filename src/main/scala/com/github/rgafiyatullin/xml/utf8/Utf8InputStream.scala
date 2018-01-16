package com.github.rgafiyatullin.xml.utf8

import java.nio.charset.StandardCharsets
import scala.collection.immutable.Queue

trait Utf8Error extends Exception
object Utf8Error {
  case object CharComplete extends Utf8Error
  final case class InvalidLeadingByte(b: Byte) extends Utf8Error
  final case class InvalidNonLeadingByte(b: Byte) extends Utf8Error
}

trait Utf8InputStream {
  def out: (Option[Char], Utf8InputStream)
  def in(b: Byte): Either[Utf8Error, Utf8InputStream]
}

object Utf8InputStream {

  def empty: Utf8InputStream = Empty

  case object Empty extends Utf8InputStream {
    override def in(b: Byte): Either[Utf8Error, Utf8InputStream] =
      bytesExpectedByFirstByte(b) match {
        case 0 =>
          Right(CompleteChar(b.toChar))
        case some if some >= 1 && some <= 3 =>
          Right(ExpectBytes(some, Queue(b)))
        case _ =>
          Left(Utf8Error.InvalidLeadingByte(b))
      }

    override def out: (Option[Char], Utf8InputStream) = (None, this)
  }

  final case class CompleteChar(c: Char) extends Utf8InputStream {
    override def in(b: Byte): Either[Utf8Error, Utf8InputStream] =
      Left(Utf8Error.CharComplete)

    override def out: (Option[Char], Utf8InputStream) = (Some(c), Empty)
  }

  final case class ExpectBytes(bytesLeft: Int, acc: Queue[Byte]) extends Utf8InputStream {
    override def in(b: Byte): Either[Utf8Error, Utf8InputStream] =
      bytesExpectedByFirstByte(b) match {
        case -1 if bytesLeft == 1 =>
          Right(CompleteChar(toChar(acc.enqueue(b))))

        case -1 if bytesLeft > 1 =>
          Right(copy(bytesLeft = bytesLeft - 1, acc = acc.enqueue(b)))

        case invalid if invalid != -1 =>
          Left(Utf8Error.InvalidNonLeadingByte(b))
      }

    override def out: (Option[Char], Utf8InputStream) = (None, this)


    private def toChar(acc: Queue[Byte]): Char = {
      new String(acc.toArray, StandardCharsets.UTF_8).charAt(0)
    }
  }


  val query0: Byte = 0xc0.toByte // 1100 0000
  val query1: Byte = 0x80.toByte // 1000 0000
  val query2: Byte = 0xe0.toByte // 1110 0000
  val query3: Byte = 0xf0.toByte // 1111 0000
  val query4: Byte = 0xf8.toByte // 1111 1000

  val mask0: Byte = 0x80.toByte  // 1000 0000
  val mask1: Byte = 0x00.toByte  // 0000 0000
  val mask2: Byte = 0xc0.toByte  // 1100 0000
  val mask3: Byte = 0xe0.toByte  // 1110 0000
  val mask4: Byte = 0xf0.toByte  // 1111 0000

  private def bytesExpectedByFirstByte(b: Byte): Int = {
    if ((b & query1) == mask1) 0
    else if ((b & query0) == mask0 ) -1
    else if ((b & query2) == mask2) 1
    else if ((b & query3) == mask3) 2
    else if ((b & query4) == mask4) 3
    else -2
  }
}