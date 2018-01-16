package com.github.rgafiyatullin.xml.stream_writer.high_level_writer

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, Position}
import org.scalatest.{FlatSpec, Matchers}

class HighLevelWriterSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition

  "A HighLevelWriter" should "render" in {
    val rendered = Seq(
      HighLevelEvent.Comment(ep, "text"),
      HighLevelEvent.ProcessingInstrutcion(ep, "target", "content"),
      HighLevelEvent.ElementOpen(ep, "streams", "stream", "streams-namespace", Seq(
        Attribute.NsImport("streams", "streams-namespace"),
        Attribute.NsImport("", "jabber:client"),
        Attribute.Unprefixed("to", "im.&localhost"),
        Attribute.Prefixed("streams", "local-name", "value&")
      )),
      HighLevelEvent.ElementSelfClosing(ep, "streams", "features", "streams-namespace", Seq()),
      HighLevelEvent.ElementClose(ep, "streams", "stream", "streams-namespace")
    ).foldLeft(HighLevelWriter.empty)(_.in(_)).out._1.mkString

    //    println(rendered)

    rendered should be (
      "<!--text-->" +
        "<?target content?>" +
        "<streams:stream" +
        " xmlns:streams='streams-namespace'" +
        " xmlns='jabber:client'" +
        " to='im.&amp;localhost'" +
        " streams:local-name='value&amp;'" +
        ">" +
        "<streams:features" +
        "/>" +
        "</streams:stream>"
    )
  }
}
