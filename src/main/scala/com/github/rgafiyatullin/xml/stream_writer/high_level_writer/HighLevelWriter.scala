package com.github.rgafiyatullin.xml.stream_writer.high_level_writer

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, LowLevelEvent}
import com.github.rgafiyatullin.xml.stream_writer.low_level_writer.LowLevelWriter

object HighLevelWriter {
  def empty: HighLevelWriter = HighLevelWriter(LowLevelWriter.empty)
}

case class HighLevelWriter(llWriter: LowLevelWriter) {
  def out: (Seq[String], HighLevelWriter) = {
    val (rendered, nextLLWriter) = llWriter.out
    (rendered, copy(llWriter = nextLLWriter))
  }

  def in(highLevelEvent: HighLevelEvent): HighLevelWriter = {
    val llEvents = highLevelEvent match {
      case HighLevelEvent.CData(pos, text) =>
        Seq(LowLevelEvent.CData(pos, text))

      case HighLevelEvent.PCData(pos, text) =>
        Seq(LowLevelEvent.PCData(pos, text))

      case HighLevelEvent.Whitespace(pos, text) =>
        Seq(LowLevelEvent.Whitespace(pos, text))

      case HighLevelEvent.ProcessingInstrutcion(pos, target, content) =>
        Seq(LowLevelEvent.ProcessingInstruction(pos, target, content))

      case HighLevelEvent.Comment(pos, text) =>
        Seq(LowLevelEvent.Comment(pos, text))

      case HighLevelEvent.ElementClose(pos, prefix, localName, namespace) =>
        Seq(LowLevelEvent.CloseElement(pos, prefix, localName))

      case HighLevelEvent.ElementOpen(pos, prefix, localName, namespace, attributes) =>
        Seq(LowLevelEvent.OpenElementStart(pos, prefix, localName)) ++
          attributes.map {
            case Attribute.NsImport(impPrefix, impNamespace) =>
              LowLevelEvent.AttributeXmlns(pos, impPrefix, impNamespace)

            case Attribute.Prefixed(attrPrefix, attrLocalName, attrValue) =>
              LowLevelEvent.PrefixedAttribute(pos, attrPrefix, attrLocalName, attrValue)

            case Attribute.Unprefixed(attrName, attrValue) =>
              LowLevelEvent.UnprefixedAttribute(pos, attrName, attrValue)
          } ++
            Seq(LowLevelEvent.OpenElementEnd(pos))

      case HighLevelEvent.ElementSelfClosing(pos, prefix, localName, namespace, attributes) =>
        Seq(LowLevelEvent.OpenElementStart(pos, prefix, localName)) ++
          attributes.map {
            case Attribute.NsImport(impPrefix, impNamespace) =>
              LowLevelEvent.AttributeXmlns(pos, impPrefix, impNamespace)

            case Attribute.Prefixed(attrPrefix, attrLocalName, attrValue) =>
              LowLevelEvent.PrefixedAttribute(pos, attrPrefix, attrLocalName, attrValue)

            case Attribute.Unprefixed(attrName, attrValue) =>
              LowLevelEvent.UnprefixedAttribute(pos, attrName, attrValue)
          } ++
            Seq(LowLevelEvent.OpenElementSelfClose(pos))

    }
    val nextLLWriter = llEvents.foldLeft(llWriter)(_.in(_))
    copy(llWriter = nextLLWriter)
  }
}
