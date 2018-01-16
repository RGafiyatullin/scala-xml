package com.github.rgafiyatullin.xml.stream_writer.low_level_writer

import com.github.rgafiyatullin.xml.common.{LowLevelEvent, Position}
import org.scalatest.{FlatSpec, Matchers}

class LowLevelWriterSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition
  "LowLevelWriter" should "render" in {
    val rendered = Seq(
      LowLevelEvent.Comment(ep, "text"),
      LowLevelEvent.ProcessingInstruction(ep, "target", "content"),
      LowLevelEvent.OpenElementStart(ep, "streams", "stream"),
      LowLevelEvent.AttributeXmlns(ep, "streams", "streams-namespace"),
      LowLevelEvent.AttributeXmlns(ep, "", "jabber:client"),
      LowLevelEvent.UnprefixedAttribute(ep, "to", "im.localhost"),
      LowLevelEvent.PrefixedAttribute(ep, "streams", "local-name", "value"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.OpenElementStart(ep, "streams", "features"),
      LowLevelEvent.OpenElementSelfClose(ep),
      LowLevelEvent.OpenElementStart(ep, "", "presence"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.OpenElementStart(ep, "", "x"),
      LowLevelEvent.OpenElementSelfClose(ep),
      LowLevelEvent.CloseElement(ep, "", "presence"),
      LowLevelEvent.CloseElement(ep, "streams", "stream")
    ).foldLeft(LowLevelWriter.empty)(_.in(_)).out._1.mkString

//    println(rendered)

    rendered should be (
      "<!--text-->" +
        "<?target content?>" +
        "<streams:stream" +
        " xmlns:streams='streams-namespace'" +
        " xmlns='jabber:client'" +
        " to='im.localhost'" +
        " streams:local-name='value'" +
        ">" +
        "<streams:features" +
        "/>" +
        "<presence" +
        ">" +
        "<x/>" +
        "</presence>" +
        "</streams:stream>"
    )
  }
}
