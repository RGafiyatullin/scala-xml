package com.github.rgafiyatullin.xml.stream_parser.tokenizer

import com.github.rgafiyatullin.xml.common.Position

sealed trait TokenizerError extends Exception {
  def position: Position
  def description: String

  override def getMessage: String = description
}

object TokenizerError {

  final case class InputBufferUnderrun(
                                        override val position: Position) extends TokenizerError {
    override def description: String =
      "unexpected end of input stream"
  }

  final case class UnexpectedChar(
                                   override val position: Position,
                                   char: Char,
                                   state: State) extends TokenizerError {
    override def description: String =
      "unexpected char '%c' when in state '%s'; position: %s".format(char, state, position)
  }

}
