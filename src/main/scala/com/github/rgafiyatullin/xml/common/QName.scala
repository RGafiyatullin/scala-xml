package com.github.rgafiyatullin.xml.common

object QName {
  val empty = QName("", "")
}

/**
  * Each XML-element has a qualified name consisting of two parts:
  * - namespace
  * - local name
  *
  * @param ns
  * @param localName
  */
case class QName(ns: String, localName: String) {
  def isEmpty: Boolean = ns.isEmpty && localName.isEmpty
}
