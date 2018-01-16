package com.github.rgafiyatullin.xml.dom_query

import org.scalatest.{FlatSpec, Matchers}
import Implicits._
import com.github.rgafiyatullin.xml.dom.Node

class DomQueryUpdateTest extends FlatSpec with Matchers {
  import Fixtures._

  def touch(n: Node): Node = n.withAttribute("touched", Some("1"))
  def isTouched(n: Node): Boolean = n.attribute("touched").contains("1")

  "Predicate.Any" should "update all immediate children" in {
    val p0 = inputs.p0.update(Predicate.Any)(touch)
    p0.children.forall(isTouched) should be (true)

    val p1 = inputs.p0.update(Predicate.Any)(touch)
    p1.children.forall(isTouched) should be (true)
  }

  "Predicate.Any / Predicate.Any" should "update second level children" in {
    val p0 = inputs.p0.update(Predicate.Any / Predicate.Any)(touch)
    p0.children.forall(_.children.forall(isTouched)) should be (true)

    val p1 = inputs.p1.update(Predicate.Any / Predicate.Any)(touch)
    p1.children.forall(_.children.forall(isTouched)) should be (true)
  }
}
