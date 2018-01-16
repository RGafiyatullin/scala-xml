package com.github.rgafiyatullin.xml.dom_query

import org.scalatest.{FlatSpec, Matchers}
import Implicits._

class DomQueryUpsertTest extends FlatSpec with Matchers {
  import Fixtures._

  "Upsert" should "create non-existent element" in {
    val p0 = inputs.p0.upsert(names.xmu / (names.xmus and attrs.status.s110))(Some(_))
    p0.children.exists(
      xmu =>
        xmu.qName == names.xmu
          && xmu.children.exists(
          status =>
            status.qName == names.xmus
              && status.attribute("status").contains("110"))) should be (true)
  }
}
