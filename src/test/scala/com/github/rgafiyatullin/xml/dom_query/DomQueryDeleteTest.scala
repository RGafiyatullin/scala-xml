package com.github.rgafiyatullin.xml.dom_query

import org.scalatest.{FlatSpec, Matchers}
import Implicits._

class DomQueryDeleteTest extends FlatSpec with Matchers {
  import Fixtures._

  "Predicate.Any" should "delete all immediate children" in {
    val p0 = inputs.p0 delete Predicate.Any
    p0.children should be (empty)

    val p1 = inputs.p1 delete Predicate.Any
    p1.children should be (empty)
  }

  "Predicate.Any / Predicate.Any" should "delete second level children" in {
    val p0 = inputs.p0 delete Predicate.Any / Predicate.Any
    p0.children.size should be (inputs.p0.children.size)
    p0.children.forall(_.children.isEmpty) should be (true)

    val p1 = inputs.p1 delete Predicate.Any / Predicate.Any
    p1.children.size should be (inputs.p1.children.size)
    p1.children.forall(_.children.isEmpty) should be (true)
  }

  "Predicate.QNameIs" should "delete by QName" in {
    val p0 = inputs.p0 delete names.xmu
    p0.children should be (empty)

    val p1 = inputs.p1 delete names.xmu
    p1.children should be (inputs.p1.children.tail)
  }

  "QNames" should "chain into Path" in {
    val p0 = inputs.p0 delete names.xmu / names.xmus
    p0.children should be (empty)

    val p1 = inputs.p1 delete names.xmu / names.xmus
    p1.children.head.children.size should be (inputs.p1.children.head.children.size - 2)
  }

  "Predicate.UnprefixedAttributeIs" should "select by attr vlaue" in {
    val p0 = inputs.p0 delete names.xmu / (names.xmus and attrs.status.s201)
    p0.children should be (empty)

    val p1 = inputs.p1 delete names.xmu / (names.xmus and attrs.status.s201)
    p1.children.head.children.size should be (inputs.p1.children.head.children.size - 1)
  }
}
