package com.github.rgafiyatullin.xml.common

sealed trait LowLevelEvent {
  def position: Position
}

object LowLevelEvent {

  final case class Comment(position: Position, text: String) extends LowLevelEvent

  final case class ProcessingInstruction(position: Position, target: String, content: String) extends LowLevelEvent

  final case class OpenElementStart(position: Position, prefix: String, localName: String) extends LowLevelEvent

  final case class AttributeXmlns(position: Position, prefix: String, namespace: String) extends LowLevelEvent

  final case class PrefixedAttribute(position: Position, prefix: String, localName: String, value: String) extends LowLevelEvent

  final case class UnprefixedAttribute(position: Position, name: String, value: String) extends LowLevelEvent

  final case class OpenElementEnd(position: Position) extends LowLevelEvent

  final case class OpenElementSelfClose(position: Position) extends LowLevelEvent

  final case class Whitespace(position: Position, text: String) extends LowLevelEvent

  final case class PCData(position: Position, text: String) extends LowLevelEvent

  final case class CData(position: Position, text: String) extends LowLevelEvent

  final case class CloseElement(position: Position, prefix: String, localName: String) extends LowLevelEvent

}
