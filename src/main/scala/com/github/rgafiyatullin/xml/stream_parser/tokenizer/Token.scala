package com.github.rgafiyatullin.xml.stream_parser.tokenizer

import com.github.rgafiyatullin.xml.common.Position

sealed trait Token {
  def position: Position
}

object Token {
  // '<?'
  final case class PIOpen(position: Position) extends Token

  final case class PITarget(position: Position, target: String) extends Token

  final case class PIContent(position: Position, content: String) extends Token

  // '?>'
  final case class PIClose(position: Position) extends Token

  // '<'
  final case class Lt(position: Position) extends Token

  // '</'
  final case class LtSlash(position: Position) extends Token

  // '>'
  final case class Gt(position: Position) extends Token

  // '/>'
  final case class SlashGt(position: Position) extends Token

  // '='
  final case class EqSign(position: Position) extends Token

  final case class XmlName(position: Position, name: String) extends Token

  final case class AttributeValue(position: Position, value: String) extends Token

  // '<!--'
  final case class CommentOpen(position: Position) extends Token

  // a piece of text inside a comment
  final case class CommentText(position: Position, text: String) extends Token

  // '-->'
  final case class CommentClose(position: Position) extends Token


  final case class CDataOpen(position: Position) extends Token

  final case class CDataContent(position: Position, acc: String) extends Token

  final case class CDataClose(position: Position) extends Token

  final case class Character(position: Position, char: Char) extends Token

  final case class Whitespace(position: Position, char: Char) extends Token

}
