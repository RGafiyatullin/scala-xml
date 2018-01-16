package com.github.rgafiyatullin.xml.dom_query

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import com.github.rgafiyatullin.xml.dom.{Element, Node}

import scala.collection.immutable.Queue

object DomQuery {
  type UpdateFunc = Node => Node
  type UpsertFunc = Option[Node] => Option[Node]

  case class Select(path: Path) extends DomQuery {
    def apply(node: Node): Seq[Node] =
      processChildren(Queue.empty, node, path)

    private def processNode(acc0: Queue[Node], node: Node, path: Path): Queue[Node] =
      (path.matches(node), path.isLast) match {
        case (true, true) =>
          acc0.enqueue(node)
        case (true, false) =>
          val nextPath = path.next
          processChildren(acc0, node, nextPath)
        case (_, _) =>
          acc0
      }

    private def processChildren(acc0: Queue[Node], node: Node, nextPath: Path): Queue[Node] =
      node.children
        .foldLeft(acc0)(processNode(_, _, nextPath))
  }

  case class Delete(path: Path) extends DomQuery {
    def apply(node: Node): Node =
      processChildren(node, path)


    private def processChildren(node: Node, nextPath: Path): Node =
      node.withChildren(
        node.children.map(
          processNode(_, nextPath))
          .collect({
            case Some(ch) => ch}))

    private def processNode(node: Node, path: Path): Option[Node] =
      (path.matches(node), path.isLast) match {
        case (true, true) =>
          None

        case (true, false) =>
          val nextPath = path.next
          Some(processChildren(node, nextPath))

        case (_, _) =>
          Some(node)
      }

  }

  case class Update(path: Path, f: Node => Node) extends DomQuery {
    def apply(node: Node): Node =
      processChildren(node, path)

    private def processNode(node: Node, path: Path): Node = {
      (path.matches(node), path.isLast) match {
        case (true, true) =>
          f(node)

        case (true, false) =>
          processChildren(node, path.next)

        case (_, _) =>
          node
      }
    }

    private def processChildren(node: Node, nextPath: Path): Node =
      node.withChildren(
        node.children.map(
          processNode(_, nextPath)))


  }
  case class Upsert(path: Path, f: Node => Option[Node]) extends DomQuery {
    def apply(node: Node): Node =
      processChildren(node, path)


    def processNode(node: Node, path: Path): Option[Node] =
      (path.matches(node), path.isLast) match {
        case (true, true) =>
          f(node)

        case (true, false) =>
          Some(processChildren(node, path.next))

        case (_, _) =>
          Some(node)
      }

    def createAndProcessNode(path: Path): Option[Node] =
      for {
        predicate <- path.headOption
        qName <- predicate.qNameOption
        attributes = predicate.attributes
        node <-
          if (path.isLast)
            f(Element(qName, attributes, Seq()))
          else
            for {child <- createAndProcessNode(path.next)}
              yield Element(qName, attributes, Seq(child))
      }
        yield node


    def processChildren(node: Node, nextPath: Path): Node = {
      val (affectedNodes, children1) =
        node.children
          .foldLeft(0, Queue.empty[Option[Node]]) {
            case ((count, acc), ch) if nextPath.matches(ch) =>
              (count + 1, acc.enqueue(processNode(ch, nextPath)))

            case ((count, acc), ch) =>
              (count, acc.enqueue(Some(ch)))
          }
      val children2 =
        if (affectedNodes != 0)
          children1
        else
          children1.enqueue(createAndProcessNode(nextPath))

      node
        .withChildren(
          children2.collect { case Some(ch) => ch })
    }

  }
}

trait DomQuery
