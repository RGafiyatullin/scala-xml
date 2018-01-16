package com.github.rgafiyatullin.xml.dom_query

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import com.github.rgafiyatullin.xml.dom.Node

import scala.collection.immutable.Queue

trait Predicate {
  /**
    * Compose two predicates into a Predicate.Conj
    *
    * @param that
    * @return
    */
  def and(that: Predicate): Predicate = Predicate.Conj(this, that)

  /**
    * Compose two predicates into a Predicate.Disj
    *
    * @param that
    * @return
    */
  def or(that: Predicate): Predicate = Predicate.Disj(this, that)
//  def not: Predicate = Predicate.Neg(this)

  /**
    * Application of the predicate to the node
    *
    * @param node
    * @return true if the node satisfies the predicate, false — otherwise
    */
  def apply(node: Node): Boolean

  def qNameOption: Option[QName]
  def attributes: Seq[Attribute]

  /**
    * Compose two predicates into a Path
    *
    * @param next
    * @return
    */
  def /(next: Predicate): Path = Path(Queue(this, next))
}

object Predicate {
  /**
    * A constant predicate that is satisfied by any node
    */
  val Any: Predicate = Predicate.Const(true)

  /**
    * An and-composition of two predicates: is satisfied iff both sub-predicates are satisfied (short circuit)
    *
    * @param a
    * @param b
    */
  final case class Conj(a: Predicate, b: Predicate) extends Predicate {
    override def apply(node: Node): Boolean =
      a(node) && b(node)

    override def qNameOption: Option[QName] =
      a.qNameOption.orElse(b.qNameOption)

    override def attributes: Seq[Attribute] =
      a.attributes ++ b.attributes
  }

  /**
    * An or-composition of two predicates: is satisfied if at least one of sub-predicates is satisfied (short circuit)
    * @param a
    * @param b
    */
  final case class Disj(a: Predicate, b: Predicate) extends Predicate {
    override def apply(node: Node): Boolean =
      a(node) || b(node)

    override def qNameOption: Option[QName] =
      a.qNameOption.orElse(b.qNameOption)

    override def attributes: Seq[Attribute] =
      a.attributes ++ b.attributes
  }

  /**
    * A negation of the predicate: is satisfied iff the sub-predicate is not satisfied
    * @param a
    */
  final case class Neg(a: Predicate) extends Predicate {
    override def apply(node: Node): Boolean =
      !a(node)

    override def qNameOption: Option[QName] =
      a.qNameOption

    override def attributes: Seq[Attribute] =
      a.attributes
  }

  /**
    * A constant predicate
    *
    * @param b
    */
  final case class Const(b: Boolean) extends Predicate {
    override def qNameOption: Option[QName] = None
    override def attributes: Seq[Attribute] = Seq()
    override def apply(node: Node): Boolean = b
  }

  /**
    * QName match predicate: is satisfied iff the node's qName is equal to the `qn`
    * @param qn
    */
  final case class QNameIs(qn: QName) extends Predicate {
    override def apply(node: Node): Boolean =
      node.qName == qn

    override def qNameOption: Option[QName] =
      Some(qn)

    override def attributes: Seq[Attribute] =
      Seq()
  }

  /**
    * Ns match predicate: is satisfied iff the node's qName.ns is equal to the `ns`
    * @param ns
    */
  final case class NsIs(ns: String) extends Predicate {
    override def qNameOption: Option[QName] =
      None

    override def attributes: Seq[Attribute] =
      Seq.empty

    override def apply(node: Node): Boolean =
      node.qName.ns == ns
  }

  /**
    * LocalName match predicate: is satisfied iff the node's qName.localName is equal to the `localName`
    * @param localName
    */
  final case class LocalNameIs(localName: String) extends Predicate {
    override def qNameOption: Option[QName] =
      None

    override def attributes: Seq[Attribute] =
      Seq.empty

    override def apply(node: Node): Boolean =
      node.qName.localName == localName
  }

  /**
    * An arbitrary function predicate.
    *
    * @param predicate
    */
  final case class Func(predicate: Node => Boolean) extends Predicate {
    override def qNameOption: Option[QName] =
      None

    override def attributes: Seq[Attribute] =
      Seq.empty

    override def apply(node: Node): Boolean =
      predicate(node)
  }

  /**
    * An unprefixed-attr match predicate:
    *  is satisfied iff the value of an unprefixed attribute with the given name (`key`) is equal to the specified `value`
    *
    * @param key
    * @param value
    */
  final case class UnprefixedAttrIs(key: String, value: String) extends Predicate {
    override def apply(node: Node): Boolean =
      node.attribute(key).contains(value)

    override def qNameOption: Option[QName] =
      None

    override def attributes: Seq[Attribute] =
      Seq(Attribute.Unprefixed(key, value))
  }
}
