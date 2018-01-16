package com.github.rgafiyatullin.xml.common

/**
  * The implementations of this trait represent attributes that an XML-element can have.
  */
sealed trait Attribute

object Attribute {
  /**
    * Represents a namespace-import attribute.
    *
    * `NsImport("jc", "jabber:client")` is equivalent to `xmlns:jc='jabber:client'`
    *
    * @param prefix
    * @param namespace
    */
  final case class NsImport(prefix: String, namespace: String) extends Attribute

  /**
    * Represents a prefixed attribute.
    *
    * `Prefixed("xmpp", "version", "1.0")` is equivalent to `xmpp:version='1.0'`
    * @param prefix
    * @param localName
    * @param value
    */
  final case class Prefixed(prefix: String, localName: String, value: String) extends Attribute

  /**
    * Represents an unprefixed attribute
    *
    * `Unprefixed("id", "presence:initial")` is equivalent to `id='presence:initial'`
    * @param name
    * @param value
    */
  final case class Unprefixed(name: String, value: String) extends Attribute

}

