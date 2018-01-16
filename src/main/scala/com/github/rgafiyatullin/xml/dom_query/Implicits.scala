package com.github.rgafiyatullin.xml.dom_query

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import com.github.rgafiyatullin.xml.dom.Node

import scala.collection.immutable.Queue

object Implicits {
  /**
    * A set of extension methods on a DOM-node.
    * @see NodeWithQueries
    *
    * @param node
    */
  implicit class NodeToNodeWithQueries(node: Node) extends NodeWithQueries(node)

  /**
    * An implicit conversion of a single Predicate into a Path
    *
    * @param predicate
    * @return
    */
  implicit def predicateToPath(predicate: Predicate): Path =
    Path(Queue(predicate))

  /**
    * An implicit conversion of a boolean into a Predicate.Const(b)
    *
    * @param b
    * @return
    */
  implicit def booleanToPredicate(b: Boolean): Predicate =
    Predicate.Const(b)

  /**
    * An implicit conversion of an Attribute.Unprefix into a Predicate.UnprefixedAttrIs(name, value)
    *
    * @param attr
    * @return
    */
  implicit def attributeToPredicate(attr: Attribute.Unprefixed): Predicate =
    Predicate.UnprefixedAttrIs(attr.name, attr.value)

  /**
    * An implicit conversion of a QName into Predicate.QNameIs(qn)
    *
    * @param qn
    * @return
    */
  implicit def qNameToPredicate(qn: QName): Predicate =
    Predicate.QNameIs(qn)
}
