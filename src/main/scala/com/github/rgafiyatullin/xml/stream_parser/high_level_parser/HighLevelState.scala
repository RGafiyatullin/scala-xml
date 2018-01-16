package com.github.rgafiyatullin.xml.stream_parser.high_level_parser

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, LowLevelEvent, Position}

import scala.collection.immutable.Queue

sealed trait HighLevelState {
  type ProcessLowLevelEvent = PartialFunction[LowLevelEvent, (Seq[HighLevelEvent], HighLevelState)]

  def processLowLevelEvent: ProcessLowLevelEvent


  protected def ensurePrefix(prefix: String, pos: Position, nsImports: NsImportCtx): Unit =
    if (nsImports.resolvePrefix(prefix).isEmpty)
      throw HighLevelParserError.UndeclaredPrefix(pos, prefix)

  def withNsImportCtx(f: NsImportCtx => NsImportCtx): HighLevelState
}

object HighLevelState {

  def initialState: HighLevelState = Normal(NsImportCtx.empty, None)

  final case class EndOfDocument() extends HighLevelState {
    override def processLowLevelEvent: ProcessLowLevelEvent = ???

    override def withNsImportCtx(f: (NsImportCtx) => NsImportCtx) = this
  }

  final case class Normal(nsImports: NsImportCtx, parent: Option[Normal]) extends HighLevelState {
    override def processLowLevelEvent: ProcessLowLevelEvent = {
      case LowLevelEvent.OpenElementStart(pos, prefix, localName) =>
        val nextState = OpeningElement(
          prefix, localName, nsImports.push, Queue.empty, this)
        (Seq(), nextState)

      case LowLevelEvent.CloseElement(pos, prefix, localName) =>
        ensurePrefix(prefix, pos, nsImports)
        val nextState = parent.getOrElse(EndOfDocument())
        val event = HighLevelEvent.ElementClose(
                      pos, prefix, localName, nsImports.resolvePrefix(prefix).get)
        (Seq(event), nextState)

      case LowLevelEvent.ProcessingInstruction(pos, target, content) =>
        (Seq(HighLevelEvent.ProcessingInstrutcion(pos, target, content)), this)

      case LowLevelEvent.CData(pos, text) =>
        (Seq(HighLevelEvent.CData(pos, text)), this)

      case LowLevelEvent.PCData(pos, text) =>
        (Seq(HighLevelEvent.PCData(pos, text)), this)

      case LowLevelEvent.Whitespace(pos, text) =>
        (Seq(HighLevelEvent.Whitespace(pos, text)), this)

      case LowLevelEvent.Comment(pos, text) =>
        (Seq(HighLevelEvent.Comment(pos, text)), this)
    }

    override def withNsImportCtx(f: (NsImportCtx) => NsImportCtx) = copy(nsImports = f(nsImports))
  }

  final case class OpeningElement(
                                   prefix: String,
                                   localName: String,
                                   nsImports: NsImportCtx,
                                   attributes: Queue[Attribute],
                                   parent: Normal) extends HighLevelState {
    override def processLowLevelEvent: ProcessLowLevelEvent = {
      case LowLevelEvent.OpenElementEnd(pos) =>
        ensurePrefixes(pos)
        val nextState = Normal(nsImports, Some(parent))
        (Seq(completeEvent(pos)), nextState)

      case LowLevelEvent.AttributeXmlns(pos, newPrefix, newNamespace) =>
        val nextAttributes = attributes.enqueue(Attribute.NsImport(newPrefix, newNamespace))
        val nextNsImports = nsImports.add(pos, newPrefix, newNamespace)
        val nextState = copy(attributes = nextAttributes, nsImports = nextNsImports)
        (Seq(), nextState)

      case LowLevelEvent.PrefixedAttribute(pos, attrPrefix, attrLocalName, attrValue) =>
        val nextAttributes = attributes.enqueue(Attribute.Prefixed(attrPrefix, attrLocalName, attrValue))
        val nextState = copy(attributes = nextAttributes)
        (Seq(), nextState)

      case LowLevelEvent.UnprefixedAttribute(pos, attrName, attrValue) =>
        val nextAttributes = attributes.enqueue(Attribute.Unprefixed(attrName, attrValue))
        val nextState = copy(attributes = nextAttributes)
        (Seq(), nextState)

      case LowLevelEvent.OpenElementSelfClose(pos) =>
        ensurePrefixes(pos)
        (Seq(HighLevelEvent.ElementSelfClosing(
          pos, prefix, localName, nsImports.resolvePrefix(prefix).get, attributes)), parent)


    }

    private def ensurePrefixes(pos: Position): Unit = {
      ensurePrefix(prefix, pos, nsImports)

      attributes.foreach {
        case pa: Attribute.Prefixed =>
          ensurePrefix(pa.prefix, pos, nsImports)
        case _ =>
          ()
      }
    }

    private def completeEvent(pos: Position): HighLevelEvent =
      HighLevelEvent.ElementOpen(
        pos,  prefix, localName, nsImports.resolvePrefix(prefix).get,
        attributes
      )

    override def withNsImportCtx(f: (NsImportCtx) => NsImportCtx) = copy(nsImports = f(nsImports))
  }

}





