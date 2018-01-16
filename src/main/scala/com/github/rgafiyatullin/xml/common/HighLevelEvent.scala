package com.github.rgafiyatullin.xml.common

sealed trait HighLevelEvent

object HighLevelEvent {

  /**
    * Represents a comment within an XML-stream.
    *
    * Equivalent to `<!-- text -->`
    * @param position
    * @param text
    */
  final case class Comment(
    position: Position,
    text: String) extends HighLevelEvent

  /**
    * Represents a whitespace within an XML-stream.
    *
    * @param position
    * @param text
    */
  final case class Whitespace(
    position: Position,
    text: String) extends HighLevelEvent

  /**
    * Represents a processing instruction within an XML-stream.
    *
    * Equivalent to `<?target content ?>`
    * @param position
    * @param target
    * @param content
    */
  final case class ProcessingInstrutcion(
    position: Position,
    target: String,
    content: String) extends HighLevelEvent

  /**
    * Represents a CData-node
    *
    * <![CDATA[text]]>
    * @param position
    * @param text
    */
  final case class CData(
    position: Position,
    text: String) extends HighLevelEvent

  /**
    * Represents a PCData (a text node)
    * @param position
    * @param text
    */
  final case class PCData(
    position: Position,
    text: String) extends HighLevelEvent

  /**
    * Represents an openning XML-element tag
    *
    * Equivalent to `<prefix:localName xmlns:prefix='namespace' ...attributes... >`
    * @param position
    * @param prefix
    * @param localName
    * @param namespace
    * @param attributes
    */
  final case class ElementOpen(
    position: Position,
    prefix: String,
    localName: String,
    namespace: String,
    attributes: Seq[Attribute]) extends HighLevelEvent

  /**
    * Represents a closing XML-element tag
    *
    * Equivalent to `</prefix:localName>`
    * @param position
    * @param prefix
    * @param localName
    * @param namespace
    */
  final case class ElementClose(
    position: Position,
    prefix: String,
    localName: String,
    namespace: String) extends HighLevelEvent

  /**
    * Represents a self-closing XML-element tag.
    *
    * Equivalent to `<prefix:localName xmlns:prefix='namespace' ...attributes... />`
    * @param position
    * @param prefix
    * @param localName
    * @param namespace
    * @param attributes
    */
  final case class ElementSelfClosing(
    position: Position,
    prefix: String,
    localName: String,
    namespace: String,
    attributes: Seq[Attribute]) extends HighLevelEvent
}
