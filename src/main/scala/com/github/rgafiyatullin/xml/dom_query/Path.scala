package com.github.rgafiyatullin.xml.dom_query

import com.github.rgafiyatullin.xml.dom.Node

import scala.collection.immutable.Queue

case class Path(predicates: Queue[Predicate]) {
  def headOption: Option[Predicate] = predicates.headOption
  def isLast: Boolean = predicates.nonEmpty && predicates.tail.isEmpty
  def matches(node: Node): Boolean = predicates.head(node)
  def next: Path = Path(predicates.tail)

  def /(predicate: Predicate): Path =
    Path(predicates = predicates.enqueue(predicate))
}
