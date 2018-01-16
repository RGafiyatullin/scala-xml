package com.github.rgafiyatullin.xml.stream_writer.low_level_writer

import com.github.rgafiyatullin.xml.common.LowLevelEvent

import scala.collection.immutable.Queue

object LowLevelWriter {
  def empty: LowLevelWriter = LowLevelWriter(Queue.empty)
}

case class LowLevelWriter(outBuffer: Queue[String]) {
  private def sanitizeCDataContent(s: String): String =
    s.replace("]]>", "]]]><![CDATA[]>")

  private def sanitizePCDataContent(s: String): String =
    s.foldLeft(new StringBuilder()){
      case (b, ch) =>
        ch match {
          case '&' => b.append("&amp;")
          case '<' => b.append("&lt;")
          case '>' => b.append("&gt;")
          case '"' => b.append("&quot;")
          case '\'' => b.append("&apos;")
          case other => b.append(ch)
        }
    }.toString()

  def in(lowLevelEvent: LowLevelEvent): LowLevelWriter = {
    val eventRendered = lowLevelEvent match {
      case LowLevelEvent.Whitespace(_, text) =>
        text

      case LowLevelEvent.CData(_, text) =>
        s"<![CDATA[" + sanitizeCDataContent(text) + "]]>"

      case LowLevelEvent.PCData(_, text) =>
        sanitizePCDataContent(text)

      case LowLevelEvent.Comment(_, text) =>
        s"<!--$text-->"

      case LowLevelEvent.AttributeXmlns(_, prefix, namespace) if !prefix.isEmpty =>
        s" xmlns:$prefix='$namespace'"

      case LowLevelEvent.AttributeXmlns(_, prefix, namespace) if prefix.isEmpty =>
        s" xmlns='$namespace'"

      case LowLevelEvent.OpenElementStart(_, prefix, localName) if !prefix.isEmpty =>
        s"<$prefix:$localName"

      case LowLevelEvent.OpenElementStart(_, prefix, localName) if prefix.isEmpty =>
        s"<$localName"

      case LowLevelEvent.OpenElementEnd(_) =>
        ">"

      case LowLevelEvent.CloseElement(_, prefix, localName) if !prefix.isEmpty =>
        s"</$prefix:$localName>"

      case LowLevelEvent.CloseElement(_, prefix, localName) if prefix.isEmpty =>
        s"</$localName>"

      case LowLevelEvent.OpenElementSelfClose(_) =>
        "/>"

      case LowLevelEvent.PrefixedAttribute(_, prefix, localName, value) =>
        s" $prefix:$localName='${sanitizePCDataContent(value)}'"

      case LowLevelEvent.UnprefixedAttribute(_, name, value) =>
        s" $name='${sanitizePCDataContent(value)}'"

      case LowLevelEvent.ProcessingInstruction(_, target, content) =>
        s"<?$target $content?>"
    }
    copy(outBuffer = outBuffer.enqueue(eventRendered))
  }

  def out: (Seq[String], LowLevelWriter) =
    (outBuffer, copy(outBuffer = Queue.empty))
}


