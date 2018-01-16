package com.github.rgafiyatullin.xml.common

/**
  * This trait is used to associate the xml-stream events with the position of their origin in the source.
  */
sealed trait Position {
  def bump(ch: Char): Position
  def absolute: Int
  def line: Int
  def column: Int
  def isEmpty: Boolean
  def isDefined: Boolean = !isEmpty
}

object Position {
  def withoutPosition: Position = WithoutPosition
  def initial: Position = WithPosition(0, 0, 0)
}


final case class WithPosition(
                  override val absolute: Int,
                  override val line: Int,
                  override val column: Int) extends Position
{
  override def isEmpty: Boolean = false

  override def bump(ch: Char): Position =
    ch match {
      case '\n' => newLine
      case _ => otherChar
    }

  override def toString: String =
    "[line: %d; column: %d; absolute: %d]".format(line, column, absolute)

  private def newLine: Position =
    this.copy(absolute = absolute + 1, line = line + 1, column = 0)

  private def otherChar: Position =
    this.copy(absolute = absolute + 1, column = column + 1)
}

case object WithoutPosition extends Position {
  override def isEmpty: Boolean = true

  override def bump(ch: Char): Position = this

  override def column: Int = -1

  override def line: Int = -1

  override def absolute: Int = -1

  override def toString: String = "[no position information available]"
}