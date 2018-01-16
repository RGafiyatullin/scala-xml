package com.github.rgafiyatullin.xml.dom_query

import org.scalatest.{FlatSpec, Matchers}
import Implicits._

class DomQuerySelectTest extends FlatSpec with Matchers {
  import Fixtures._

  "Predicate.Any query" should "select all immediate children" in {
    val emptySeq = inputs.p0 select Predicate.Any
    emptySeq should be (empty)

    val nonEmptySeq = inputs.p1 select Predicate.Any
    nonEmptySeq should be (inputs.p1.children)
  }

  "Predicate.Any / Predicate.Any" should "select second level children" in {
    val emptySeq = inputs.p0 select Predicate.Any / Predicate.Any
    emptySeq should be (empty)

    val nonEmptySeq = inputs.p1 select Predicate.Any / Predicate.Any
    nonEmptySeq should be (inputs.p1.children.head.children)
  }

  "Predicate.QNameIs" should "select by QName" in {
    val emptySeq = inputs.p0 select names.xmu
    emptySeq should be (empty)

    val nonEmptySeq = inputs.p1 select names.xmu
    nonEmptySeq should be (Seq(inputs.p1.children.head))
  }

  "QNames" should "chain into Path" in {
    val emptySeq = inputs.p0 select names.xmu / names.xmus
    emptySeq should be (empty)

    val nonEmptySeq = inputs.p1 select names.xmu / names.xmus
    nonEmptySeq should be (inputs.p1.children.head.children.tail)
  }

  "Predicate.UnprefixedAttributeIs" should "select by attr vlaue" in {
    val emptySeq = inputs.p0 select names.xmu / (names.xmus and attrs.status.s201)
    emptySeq should be (empty)

    val nonEmptySeq = inputs.p1 select names.xmu / (names.xmus and attrs.status.s201)
    nonEmptySeq should be (inputs.p1.children.head.children.tail.tail)
  }

  "Predicate.NsIs" should "select by NS" in {
    val emptySeq = inputs.p0 select Predicate.NsIs(names.xmu.ns) / (Predicate.NsIs(names.xmus.ns) and attrs.status.s201)
    emptySeq should be (empty)

    val nonEmptySeq = inputs.p1 select Predicate.NsIs(names.xmu.ns) / (Predicate.NsIs(names.xmus.ns) and attrs.status.s201)
    nonEmptySeq should be (inputs.p1.children.head.children.tail.tail)
  }
}
