package com.github.rgafiyatullin.xml.dom

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, Position, QName}
import com.github.rgafiyatullin.xml.stream_writer.high_level_writer.HighLevelWriter
import org.scalatest.{FlatSpec, Matchers}

class NodeToEventsSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition

  "A Node" should "decompose into HighLevelEvent-sequence" in {
    val node: Node =
      Element(
        QName("streams-namespace", "stream"),
        Seq(
          Attribute.Unprefixed("to", "im.localhost"),
          Attribute.NsImport("streams", "streams-namespace"),
          Attribute.NsImport("x", "jabber:client")
        ), Seq(
          Element(
            QName("jabber:client", "presence"),
            Seq(), Seq(
              Element(
                QName("jabber:client", "should-have-x-prefix"),
                Seq(), Seq() ),
              Element(
                QName("x", "x"),
                Seq(Attribute.NsImport("x", "x")), Seq(
                  Element(
                    QName("jabber:client", "should-not-have-x-prefix"),
                    Seq(), Seq()
                  )
                ))
            ))
        ))
    val events: Seq[HighLevelEvent] = node.toEvents

    val eventsExpected = Seq(
      HighLevelEvent.ElementOpen(ep, "streams", "stream", "streams-namespace", Seq(
        Attribute.Unprefixed("to", "im.localhost"),
        Attribute.NsImport("streams", "streams-namespace"),
        Attribute.NsImport("x", "jabber:client")
      )),
      HighLevelEvent.ElementOpen(ep, "x", "presence", "jabber:client", Seq()),
      HighLevelEvent.ElementSelfClosing(ep, "x", "should-have-x-prefix", "jabber:client", Seq()),
      HighLevelEvent.ElementOpen(ep, "x", "x", "x", Seq(
        Attribute.NsImport("x", "x")
      )),
      HighLevelEvent.ElementSelfClosing(ep, "", "should-not-have-x-prefix", "jabber:client", Seq(
        Attribute.NsImport("", "jabber:client")
      )),
      HighLevelEvent.ElementClose(ep, "x", "x", "x"),
      HighLevelEvent.ElementClose(ep, "x", "presence", "jabber:client"),
      HighLevelEvent.ElementClose(ep, "streams", "stream", "streams-namespace")
    )

    eventsExpected.zip(events).foreach {
      case (expected, actual) =>
//        println(s"exp: $expected; actual: $actual")
        actual should be (expected)
    }
  }
}
