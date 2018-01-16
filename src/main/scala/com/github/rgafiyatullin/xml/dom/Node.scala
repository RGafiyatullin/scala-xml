package com.github.rgafiyatullin.xml.dom

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, Position, QName}
import com.github.rgafiyatullin.xml.common.Attribute.Unprefixed
import com.github.rgafiyatullin.xml.stream_parser.high_level_parser.NsImportCtx
import com.github.rgafiyatullin.xml.stream_writer.high_level_writer.HighLevelWriter

import scala.collection.immutable.Queue

object Node {
  def apply(qName: QName): Node = Element(qName)
  def apply(text: String): Node = CData(text)
}

sealed trait Node {
  def qName: QName = QName.empty
  def text: String
  def attributes: Seq[Attribute] = Seq()

  @deprecated("Use `withAttributes(Seq[Attribute): Node` instead", since = "0.2.0.1")
  final def setAttributes(attrs: Seq[Attribute]): Node =
    withAttributes(attrs)

  def withAttributes(attrs: Seq[Attribute]): Node = this
  def mapAttributes(f: Attribute => Attribute): Node =
    withAttributes(attributes map f)

  def children: Seq[Node] = Seq()

  @deprecated("Use `withChildren(Seq[Node]): Node` instead", since = "0.2.0.1")
  final def setChildren(chs: Seq[Node]): Node = withChildren(chs)

  def withChildren(chs: Seq[Node]): Node = this
  def mapChildren(f: Node => Node): Node =
    withChildren(children map f)

  def withAttribute(name: String, value: String): Node =
    withAttribute(name, Some(value))

  def withAttribute(name: String, valueOption: Option[String]): Node =
    withAttributes(
      attributes.filter {
        case Attribute.Unprefixed(`name`, _) => false
        case _ => true
      } ++
        valueOption
          .map(Attribute.Unprefixed(name, _))
          .toSeq)


  @deprecated("Use `withAttribute(String, Option[String]): Node` instead", since = "0.2.0.1")
  final def setAttribute(name: String, value: Option[String]): Node =
    withAttribute(name, value)

  @deprecated("Use `withAttribute(String, String): Node` instead", since = "0.2.0.1")
  final def setAttribute(name: String, value: String): Node =
    setAttribute(name, Some(value))

  def attribute(name: String): Option[String] =
    attributes.collectFirst {
      case Unprefixed(n, v) if n == name => v }

  def toEvents: Seq[HighLevelEvent] = {
    render(Queue.empty)
  }

  def render(eq: Queue[HighLevelEvent], nsCtx: NsImportCtx = NsImportCtx.empty): Queue[HighLevelEvent]

  def rendered: String =
    render(Queue.empty)
      .foldLeft(HighLevelWriter.empty)(_.in(_))
      .out._1.mkString

  protected val emptyPosition: Position = Position.withoutPosition
}

object Element {
  def apply(qName: QName): Node = Element(qName, Seq.empty, Seq.empty)
  def apply(qName: QName, attributes: Seq[Attribute]): Node = Element(qName, attributes, Seq.empty)
}

final case class Element(override val qName: QName, override val attributes: Seq[Attribute], override val children: Seq[Node])
  extends Node
{
  def ns: String = qName.ns
  def localName: String = qName.localName

  override def text: String =
    children.map(_.text).mkString

  override def withChildren(chs: Seq[Node]): Element =
    copy(children = chs)

  override def withAttributes(attrs: Seq[Attribute]): Element =
    copy(attributes = attrs)

  override def render(eq0: Queue[HighLevelEvent], nsCtx0: NsImportCtx): Queue[HighLevelEvent] = {
    val nsCtx1 = attributes.foldLeft(nsCtx0.push){
      case (ctx, Attribute.NsImport(addPrefix, addNamespace)) =>
        ctx.add(emptyPosition, addPrefix, addNamespace)

      case (ctx, _) => ctx
    }
    val (prefix, attributesToRender) = nsCtx1.chosePrefix(ns) match {
      case Some(prefixChosen) => (prefixChosen, attributes)
      case None =>
        val attributesFiltered = attributes.filter {
          case Attribute.NsImport("", _) =>
            false

          case _ =>
            true
        }
        val attributesWithNewImport = attributesFiltered ++ Seq(Attribute.NsImport("", ns))
        ("", attributesWithNewImport)
    }
    if (children.isEmpty) {
      eq0.enqueue(
        HighLevelEvent.ElementSelfClosing(
          emptyPosition, prefix, localName, ns, attributesToRender))
    }
    else {
      val eq1 = eq0.enqueue(
        HighLevelEvent.ElementOpen(
          emptyPosition, prefix, localName, ns, attributesToRender))
      val eq2 = children.foldLeft(eq1) {
        case (eq, child) =>
          child.render(eq, nsCtx1)
      }
      eq2.enqueue(
        HighLevelEvent.ElementClose(
          emptyPosition, prefix, localName, ns))
    }
  }
}

case class CData(text: String) extends Node {
  override def render(eq: Queue[HighLevelEvent], nsCtx: NsImportCtx): Queue[HighLevelEvent] =
    eq.enqueue(HighLevelEvent.CData(emptyPosition, text))
}

case class PCData(text: String) extends Node {
  override def render(eq: Queue[HighLevelEvent], nsCtx: NsImportCtx): Queue[HighLevelEvent] =
    eq.enqueue(HighLevelEvent.PCData(emptyPosition, text))
}

case class Comment(text: String) extends Node {
  override def render(eq: Queue[HighLevelEvent], nsCtx: NsImportCtx): Queue[HighLevelEvent] =
    eq.enqueue(HighLevelEvent.Comment(emptyPosition, text))
}

case class Whitespace(text: String) extends Node {
  override def render(eq: Queue[HighLevelEvent], nsCtx: NsImportCtx): Queue[HighLevelEvent] =
    eq.enqueue(HighLevelEvent.Whitespace(emptyPosition, text))
}
