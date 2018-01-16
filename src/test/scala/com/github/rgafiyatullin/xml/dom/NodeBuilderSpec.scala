package com.github.rgafiyatullin.xml.dom

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, Position, QName}
import org.scalatest.{FlatSpec, Matchers}

class NodeBuilderSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition
  val eb = NodeBuilder.empty


  "An empty builder" should "not be complete"  in {
    eb.isComplete should be (false)
  }

  it should "get complete upon CData" in {
    val cdata = eb.in(HighLevelEvent.CData(ep, "text"))
    cdata.isComplete should be (true)
    cdata.nodeOption should be (Some(CData("text")))
  }

  it should "get complete upon PCData" in {
    val pcdata = eb.in(HighLevelEvent.PCData(ep, "text"))
    pcdata.isComplete should be (true)
    pcdata.nodeOption should be (Some(PCData("text")))
  }

  it should "get complete upon Comment" in {
    val comment = eb.in(HighLevelEvent.Comment(ep, "text"))
    comment.isComplete should be (true)
    comment.nodeOption should be (Some(Comment("text")))
  }

  it should "get complete upon SelfClosing" in {
    val comment = eb.in(HighLevelEvent.ElementSelfClosing(ep, "", "local-name", "namespace", Seq()))
    comment.isComplete should be (true)
    comment.nodeOption should be (Some(Element(QName("namespace", "local-name"), Seq(), Seq())))
  }

  it should "get complete upon a more complex example" in {
    val builder = Seq(
      HighLevelEvent.ElementOpen(ep, "", "local-name-1", "namespace-1", Seq(
        Attribute.NsImport("", "namespace"),
        Attribute.Unprefixed("attr-1", "value-1")
      )),
      HighLevelEvent.ElementOpen(ep, "", "local-name-2", "namespace-1", Seq()),
      HighLevelEvent.CData(ep, "text"),
      HighLevelEvent.ElementClose(ep, "", "local-name-2", "namespace-1"),
      HighLevelEvent.ElementOpen(ep, "", "local-name-3", "namespace-2", Seq(
        Attribute.NsImport("", "namespace-2")
      )),
      HighLevelEvent.PCData(ep, "text"),
      HighLevelEvent.ElementClose(ep, "", "local-name-3", "namespace-2"),
      HighLevelEvent.ElementSelfClosing(ep, "", "local-name-4", "namespace-3", Seq(
        Attribute.NsImport("", "namespace-3")
      )),
      HighLevelEvent.ElementClose(ep, "", "local-name-1", "namespace-1")
    ).foldLeft(eb) {
      case (b, a) =>
        b.in(a)
    }

    val expectedElement =
      Element(
        QName("namespace-1", "local-name-1"),
        Seq(
          Attribute.Unprefixed("attr-1", "value-1")
        ),
        Seq(
          Element(QName("namespace-1", "local-name-2"), Seq(), Seq(
            CData("text")
          )),
          Element(QName("namespace-2", "local-name-3"), Seq(), Seq(
            PCData("text")
          )),
          Element(QName("namespace-3", "local-name-4"), Seq(), Seq())
        ))

    builder.isComplete should be (true)
    builder.nodeOption should be (Some(expectedElement))
  }
}
