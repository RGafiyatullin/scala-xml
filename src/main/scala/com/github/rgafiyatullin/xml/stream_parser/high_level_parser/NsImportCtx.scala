package com.github.rgafiyatullin.xml.stream_parser.high_level_parser

import com.github.rgafiyatullin.xml.common.Position

import scala.annotation.tailrec

object NsImportCtx {
  def empty: NsImportCtx =
    NsImportCtx(None, Map.empty)
}

final case class NsImportCtx(parent: Option[NsImportCtx], imports: Map[String, String]) {
  lazy val reverseImports = imports.map { case (prefix, namespace) => (namespace, prefix) }

  def push: NsImportCtx =
    NsImportCtx.empty.copy(parent = Some(this))

  def resolvePrefix(prefix: String): Option[String] =
    imports
      .get(prefix)
      .orElse(
        parent.flatMap(
          _.resolvePrefix(prefix)))

  def chosePrefix(namespace: String, ignoredPrefixes: Set[String] = Set.empty): Option[String] =
    reverseImports.get(namespace) match {
      case Some(prefix) if !ignoredPrefixes.contains(prefix) =>
        Some(prefix)

      case _ =>
        parent.flatMap(
          _.chosePrefix(
            namespace,
            imports.keys
              .foldLeft(ignoredPrefixes)(_ + _)
          ) )
    }


  def rm(prefix: String): NsImportCtx =
    copy(imports = imports.filter(_._1 != prefix))

  def add(position: Position, prefix: String, namespace: String): NsImportCtx =
    imports.get(prefix) match {
      case Some(oldNamespace) =>
        throw HighLevelParserError.PrefixIsAlreadyUsed(position, prefix, oldNamespace, namespace)

      case None =>
        copy(imports = imports + (prefix -> namespace))
    }

}

