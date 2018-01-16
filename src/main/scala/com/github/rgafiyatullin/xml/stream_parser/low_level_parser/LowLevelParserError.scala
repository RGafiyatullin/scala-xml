package com.github.rgafiyatullin.xml.stream_parser.low_level_parser

import com.github.rgafiyatullin.xml.common.Position
import com.github.rgafiyatullin.xml.stream_parser.tokenizer.{Token, TokenizerError}

sealed trait LowLevelParserError extends Exception {
  def position: Position
  def description: String

  override def getMessage: String = description
}

object LowLevelParserError {
  final case class TokError(parser: LowLevelParser, tokenizerError: TokenizerError) extends LowLevelParserError {
    override def position: Position = tokenizerError.position
    override def description: String = tokenizerError.description
  }

  final case class UnexpectedToken(token: Token, state: LowLevelState) extends LowLevelParserError {
    override def position: Position = token.position

    override def description: String =
      "unexpected token %s when in state '%s'; position: %s".format(token, state, position)
  }
}
