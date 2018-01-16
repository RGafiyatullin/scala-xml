package com.github.rgafiyatullin.xml.stream_parser

import com.github.rgafiyatullin.xml.common.{LowLevelEvent, Position}
import com.github.rgafiyatullin.xml.stream_parser.low_level_parser.{LowLevelParser, LowLevelParserError}
import com.github.rgafiyatullin.xml.stream_parser.tokenizer.TokenizerError
import org.scalatest.{FlatSpec, Matchers}

class LowLevelParserSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition

  "An empty LowLevelParser" should "throw input-underrun error" in {
    val p0 = LowLevelParser.empty.withoutPosition
    ensureParserInputUnderrun(p0)
    ()
  }

  "A LowLevelParser" should "parse #1 (processing instruction)" in {
    common("<?target content?>", Seq(
      LowLevelEvent.ProcessingInstruction(ep, "target", "content")))
  }

  it should "parse #2 (comment)" in {
    common("<!--comm-ent-->", Seq(
      LowLevelEvent.Comment(ep, "comm-ent")))
  }

  it should "parse #3 (cdata)" in {
    common("<![CDATA[co]nt]]ent]]]]]>", Seq(
      LowLevelEvent.CData(ep, "co]nt]]ent]]]")))
  }

  it should "parse #4 (simple open tag)" in {
    common("<open>", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "open"),
      LowLevelEvent.OpenElementEnd(ep)))
  }

  it should "parse #5 (prefixed open tag)" in {
    common("<qualified:open>", Seq(
      LowLevelEvent.OpenElementStart(ep, "qualified", "open"),
      LowLevelEvent.OpenElementEnd(ep)))
  }

  it should "parse #6 (self closing tag)" in {
    common("<open />", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "open"),
      LowLevelEvent.OpenElementSelfClose(ep)))
  }

  it should "parse #7 (self closing tag)" in {
    common("<qualified:open />", Seq(
      LowLevelEvent.OpenElementStart(ep, "qualified", "open"),
      LowLevelEvent.OpenElementSelfClose(ep)))
  }

  it should "parse #8 (open tag with attributes)" in {
    common("<open first-attr='one' qualified:second-attr='two'>", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "open"),
      LowLevelEvent.UnprefixedAttribute(ep, "first-attr", "one"),
      LowLevelEvent.PrefixedAttribute(ep, "qualified", "second-attr", "two"),
      LowLevelEvent.OpenElementEnd(ep)))
  }

  it should "parse #9 (open tag with ns-declarations)" in {
    common("<open xmlns='namespace#1' xmlns:prefix='namespace#2'>", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "open"),
      LowLevelEvent.AttributeXmlns(ep, "", "namespace#1"),
      LowLevelEvent.AttributeXmlns(ep, "prefix", "namespace#2"),
      LowLevelEvent.OpenElementEnd(ep)))
  }

  it should "parse #10 (self-closing qualified tag with attributes and ns-declarations)" in {
    common(
      "<qualified:open xmlns='namespace#1' first-attr='one' qualified:second-attr='two' xmlns:prefix='namespace#2' />",
      Seq(
        LowLevelEvent.OpenElementStart(ep, "qualified", "open"),
        LowLevelEvent.AttributeXmlns(ep, "", "namespace#1"),
        LowLevelEvent.UnprefixedAttribute(ep, "first-attr", "one"),
        LowLevelEvent.PrefixedAttribute(ep, "qualified", "second-attr", "two"),
        LowLevelEvent.AttributeXmlns(ep, "prefix", "namespace#2"),
        LowLevelEvent.OpenElementSelfClose(ep)
      ))
  }

  it should "parse #11 (closing tag)" in {
    common("</close>", Seq(
      LowLevelEvent.CloseElement(ep, "", "close")))
  }

  it should "parse #12 (qualified closing tag)" in {
    common("</qualified:close>", Seq(
      LowLevelEvent.CloseElement(ep, "qualified", "close")))
  }

  it should "parse #13 (a complex example)" in {
    val input =
      "<streams:stream xmlns:streams='ns:streams' xmlns='jabber:client' from='router.xmppcs.iv' to='c2s-1-5222.xmppcs.iv'><streams:features><router xmlns='ns:router' /></streams:features>"

    val events: Seq[LowLevelEvent] = Seq(
      LowLevelEvent.OpenElementStart(ep, "streams", "stream"),
      LowLevelEvent.AttributeXmlns(ep, "streams", "ns:streams"),
      LowLevelEvent.AttributeXmlns(ep, "", "jabber:client"),
      LowLevelEvent.UnprefixedAttribute(ep, "from", "router.xmppcs.iv"),
      LowLevelEvent.UnprefixedAttribute(ep, "to", "c2s-1-5222.xmppcs.iv"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.OpenElementStart(ep, "streams", "features"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.OpenElementStart(ep, "", "router"),
      LowLevelEvent.AttributeXmlns(ep, "", "ns:router"),
      LowLevelEvent.OpenElementSelfClose(ep),
      LowLevelEvent.CloseElement(ep, "streams", "features")
    )
    common(input, events)
  }

  it should "pasre #14 (pcdata)" in {
    common("<text>pcdata</text>", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "text"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.PCData(ep, "pcdata"),
      LowLevelEvent.CloseElement(ep, "", "text")
    ))
  }

  it should "parse #15 (ignorable whitespaces)" in {
    common("<text>\n \t\r\n</text>", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "text"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.Whitespace(ep, "\n \t\r\n"),
      LowLevelEvent.CloseElement(ep, "", "text")
    ))
  }

  it should "parse #16 (pcdata with leading whitespaces)" in {
    common("<text>\n\tpcdata\n</text>", Seq(
      LowLevelEvent.OpenElementStart(ep, "", "text"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.PCData(ep, "\n\tpcdata\n"),
      LowLevelEvent.CloseElement(ep, "", "text")
    ))
  }

  it should "continue properly" in {
    val inputPt1 = "<a>asdfghjkl"
    val inputPt2 = "zxcvbnm</a>"

    val p0 = LowLevelParser.empty.withoutPosition.in(inputPt1)
    val p1 = p0.in(inputPt2)

    val p1_complete = checkExpectedEvents(p1)(Seq(
      LowLevelEvent.OpenElementStart(ep, "", "a"),
      LowLevelEvent.OpenElementEnd(ep),
      LowLevelEvent.PCData(ep, "asdfghjklzxcvbnm"),
      LowLevelEvent.CloseElement(ep, "", "a")
    ))
    ensureParserInputUnderrun(p1_complete)

    val p0_aboutToUnderrun = checkExpectedEvents(p0)(Seq(
      LowLevelEvent.OpenElementStart(ep, "", "a"),
      LowLevelEvent.OpenElementEnd(ep)
    ))
    val p0_underrun = ensureParserInputUnderrun(p0_aboutToUnderrun)
    val p0_feeded = p0_underrun.in(inputPt2)
    val p0_aboutToUnderrunAgain = checkExpectedEvents(p0_feeded)(Seq(
      LowLevelEvent.PCData(ep, "asdfghjklzxcvbnm"),
      LowLevelEvent.CloseElement(ep, "", "a")
    ))
    ensureParserInputUnderrun(p0_aboutToUnderrunAgain)
    ()
  }


  private def common(input: String, expectedEvents: Seq[LowLevelEvent]): Unit = {
    val p0 = LowLevelParser.empty.withoutPosition.in(input)
    val p1 = checkExpectedEvents(p0)(expectedEvents)
    ensureParserInputUnderrun(p1)
    ()
  }

  private def checkExpectedEvents(p0: LowLevelParser)(expectedEvents: Seq[LowLevelEvent]): LowLevelParser = {
    expectedEvents.foldLeft(p0) {
      case (pIn, eventExpected) =>
        val (eventActual, pOut) = pIn.out
        eventActual should be (eventExpected)
        pOut
    }
  }

  private def ensureParserInputUnderrun(p: LowLevelParser): LowLevelParser = {
    try {
      p.out
      throw new Exception("Expected input underrun to be thrown")
    } catch {
        case pe @ LowLevelParserError.TokError(
              parser,
              te @ TokenizerError.InputBufferUnderrun(_)) =>
          pe.description.nonEmpty should be (true)
          pe.position should be (ep)
          te.description.nonEmpty should be (true)
          te.position should be (ep)

          parser
      }

  }


}
