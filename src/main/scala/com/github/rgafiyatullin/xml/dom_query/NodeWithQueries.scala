package com.github.rgafiyatullin.xml.dom_query

import com.github.rgafiyatullin.xml.dom.Node

import scala.collection.immutable.Queue

class NodeWithQueries(node: Node) {
  /**
    * Runs a `DomQuery.Select` on the node
    * @param path
    * @return
    */
  def select(path: Path): Seq[Node] = {
    val query = DomQuery.Select(path)
    query(node)
  }

  /**
    * Runs a `DomQuery.Select` on the node (the predicate argument is treated as a single element Path)
    *
    * @param predicate
    * @return
    */
  def select(predicate: Predicate): Seq[Node] =
    select(Path(Queue(predicate)))

  /**
    * Runs a `DomQuery.Delete` on the node
    *
    * @param path
    * @return
    */
  def delete(path: Path): Node = {
    val query = DomQuery.Delete(path)
    query(node)
  }

  /**
    * Runs a `DomQuery.Delete` on the node (the predicate argument is treated as a single element Path)
    *
    * @param predicate
    * @return
    */
  def delete(predicate: Predicate): Node =
    delete(Path(Queue(predicate)))

  /**
    * Runs a `DomQuery.Update` on the node
    * @param path
    * @param f
    * @return
    */
  def update(path: Path)(f: Node => Node): Node = {
    val query = DomQuery.Update(path, f)
    query(node)
  }

  /**
    * Runs a `DomQuery.Update` on the node (the predicate argument is treated as a single element Path)
    * @param predicate
    * @return
    */
  def update(predicate: Predicate): (Node => Node) => Node =
    update(Path(Queue(predicate)))

  /**
    * Runs a `DomQuery.Upsert` on the node
    *
    * @param path
    * @param f
    * @return
    */
  def upsert(path: Path)(f: Node => Option[Node]): Node = {
    val query = DomQuery.Upsert(path, f)
    query(node)
  }

  /**
    * Runs a `DomQuery.Upsert` on the node (the predicate argument is treated as a single element Path)
    * @param predicate
    * @return
    */
  def upsert(predicate: Predicate): (Node => Option[Node]) => Node =
    upsert(Path(Queue(predicate)))
}
