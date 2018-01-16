package com.github.rgafiyatullin.xml.dom

import com.github.rgafiyatullin.xml.common.{Attribute, QName}
import org.scalatest.{FlatSpec, Matchers}

class ElementSpec extends FlatSpec with Matchers {
  val element = Element(QName("namespace", "local-name"), Seq(
    Attribute.NsImport("", "namespace"),
    Attribute.Unprefixed("key-1", "value-1"),
    Attribute.Unprefixed("key-2", "value-2"),
    Attribute.Unprefixed("key-3", "value-3")
  ), Seq())

  "Element" should "provide access to its attributes by name" in {
    element.attribute("key-1") should be (Some("value-1"))
    element.attribute("key-2") should be (Some("value-2"))
    element.attribute("key-3") should be (Some("value-3"))
  }

  it should "let add new attributes" in {
    val element1 = element.withAttribute("key-0", Some("value-0"))
    element1.attribute("key-1") should be (Some("value-1"))
    element1.attribute("key-2") should be (Some("value-2"))
    element1.attribute("key-3") should be (Some("value-3"))
    element1.attribute("key-0") should be (Some("value-0"))
  }

  it should "let remove attributes" in {
    val element1 = element.withAttribute("key-1", None)
    element1.attribute("key-1") should be (None)
    element1.attribute("key-2") should be (Some("value-2"))
    element1.attribute("key-3") should be (Some("value-3"))
  }

  it should "let update existing attributes" in {
    val element1 = element.withAttribute("key-1", Some("one"))
    element1.attribute("key-1") should be (Some("one"))
    element1.attribute("key-2") should be (Some("value-2"))
    element1.attribute("key-3") should be (Some("value-3"))
  }

  it should "not fail upon removing non-existent attribute" in {
    val element1 = element.withAttribute("key-0", None)
    element.attribute("key-1") should be (Some("value-1"))
    element.attribute("key-2") should be (Some("value-2"))
    element.attribute("key-3") should be (Some("value-3"))
    element.attribute("key-0") should be (None)
  }
}
