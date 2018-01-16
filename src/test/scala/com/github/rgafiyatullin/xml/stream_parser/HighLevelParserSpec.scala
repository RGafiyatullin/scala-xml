package com.github.rgafiyatullin.xml.stream_parser

import com.github.rgafiyatullin.xml.common.{Attribute, HighLevelEvent, Position}
import com.github.rgafiyatullin.xml.stream_parser.high_level_parser.{HighLevelParser, HighLevelParserError}
import com.github.rgafiyatullin.xml.stream_parser.low_level_parser.LowLevelParserError
import com.github.rgafiyatullin.xml.stream_parser.tokenizer.TokenizerError
import org.scalatest.{FlatSpec, Matchers}

class HighLevelParserSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition

  "An empty HighLevelParser" should "throw input-underrun error" in {
    val p0 = HighLevelParser.empty.withoutPosition
    ensureParserInputUnderrun(p0)
    ()
  }

  "A HighLevelParser" should "parse #1 (element open)" in {
    common("<prefix:local-name xmlns:prefix='namespace'>", Seq(
      HighLevelEvent.ElementOpen(ep, "prefix", "local-name", "namespace", Seq(Attribute.NsImport("prefix", "namespace")))
    ))
  }

  it should "parse #2 (element self-closing)" in {
    common("<prefix:local-name xmlns:prefix='namespace' />", Seq(
      HighLevelEvent.ElementSelfClosing(ep, "prefix", "local-name", "namespace", Seq(Attribute.NsImport("prefix", "namespace")))
    ))
  }

  it should "parse #3 (element open and close)" in {
    common("<prefix:local-name xmlns:prefix='namespace'></prefix:local-name>", Seq(
      HighLevelEvent.ElementOpen(ep, "prefix", "local-name", "namespace", Seq(Attribute.NsImport("prefix", "namespace"))),
      HighLevelEvent.ElementClose(ep, "prefix", "local-name", "namespace")
    ))
  }

  it should "parse #4 (processing instruction)" in {
    common("<?target content?>", Seq(
      HighLevelEvent.ProcessingInstrutcion(ep, "target", "content")
    ))
  }

  it should "parse #5 (cdata)" in {
    common("<![CDATA[text]]>", Seq(
      HighLevelEvent.CData(ep, "text")
    ))
  }

  it should "parse #6 (pcdata)" in {
    common("<text xmlns='namespace'>text</text>", Seq(
      HighLevelEvent.ElementOpen(ep, "", "text", "namespace", Seq(Attribute.NsImport("", "namespace"))),
      HighLevelEvent.PCData(ep, "text"),
      HighLevelEvent.ElementClose(ep, "", "text", "namespace")
    ))
  }

  it should "parse #7 (comment)" in {
    common("<!-- a comment goes here -->", Seq(
      HighLevelEvent.Comment(ep, " a comment goes here ")
    ))
  }

  it should "parse #8 (whitespace)" in {
    common("<whitespace xmlns='namespace'>\r\n \t</whitespace>", Seq(
      HighLevelEvent.ElementOpen(ep, "", "whitespace", "namespace", Seq(Attribute.NsImport("", "namespace"))),
      HighLevelEvent.Whitespace(ep, "\r\n \t"),
      HighLevelEvent.ElementClose(ep, "", "whitespace", "namespace")
    ))
  }

  it should "parse #9 (a complex example)" in {
    common(
      """<streams:stream xmlns:streams='http://streams' xmlns='jabber:client' from='im.localhost'>
        | <streams:features>
        |  <feature xmlns='some-namespace'/>
        |  <feature xmlns='with-cdata'><![CDATA[some-text]]></feature>
        |  <feature xmlns='with-pcdata'>some-text</feature>
        | </streams:features>
        |</streams:stream>
      """.stripMargin, Seq(
        HighLevelEvent.ElementOpen(ep, "streams", "stream", "http://streams", Seq(
          Attribute.NsImport("streams", "http://streams"),
          Attribute.NsImport("", "jabber:client"),
          Attribute.Unprefixed("from", "im.localhost") )),
        HighLevelEvent.Whitespace(ep, "\n "),
        HighLevelEvent.ElementOpen(ep, "streams", "features", "http://streams", Seq()),
        HighLevelEvent.Whitespace(ep, "\n  "),
        HighLevelEvent.ElementSelfClosing(ep, "", "feature", "some-namespace", Seq(
          Attribute.NsImport("", "some-namespace") )),
        HighLevelEvent.Whitespace(ep, "\n  "),
        HighLevelEvent.ElementOpen(ep, "", "feature", "with-cdata", Seq(
          Attribute.NsImport("", "with-cdata") )),
        HighLevelEvent.CData(ep, "some-text"),
        HighLevelEvent.ElementClose(ep, "", "feature", "with-cdata"),
        HighLevelEvent.Whitespace(ep, "\n  "),
        HighLevelEvent.ElementOpen(ep, "", "feature", "with-pcdata", Seq(
          Attribute.NsImport("", "with-pcdata") )),
        HighLevelEvent.PCData(ep, "some-text"),
        HighLevelEvent.ElementClose(ep, "", "feature", "with-pcdata"),
        HighLevelEvent.Whitespace(ep, "\n "),
        HighLevelEvent.ElementClose(ep, "streams", "features", "http://streams"),
        HighLevelEvent.Whitespace(ep, "\n"),
        HighLevelEvent.ElementClose(ep, "streams", "stream", "http://streams")
      ))
  }

  it should "parse #9 (a complex example with unparsed rubbish accessed via .inputBuffer)" in {
    val input =
      """<streams:stream xmlns:streams='http://streams' xmlns='jabber:client' from='im.localhost'>
        | <streams:features>
        |  <feature xmlns='some-namespace'/>
        |  <feature xmlns='with-cdata'><![CDATA[some-text]]></feature>
        |  <feature xmlns='with-pcdata'>some-text</feature>
        | </streams:features>
        |</streams:stream>
        |Some rubbish goes here...""".stripMargin

    val expectedEvents = Seq(
        HighLevelEvent.ElementOpen(ep, "streams", "stream", "http://streams", Seq(
          Attribute.NsImport("streams", "http://streams"),
          Attribute.NsImport("", "jabber:client"),
          Attribute.Unprefixed("from", "im.localhost") )),
        HighLevelEvent.Whitespace(ep, "\n "),
        HighLevelEvent.ElementOpen(ep, "streams", "features", "http://streams", Seq()),
        HighLevelEvent.Whitespace(ep, "\n  "),
        HighLevelEvent.ElementSelfClosing(ep, "", "feature", "some-namespace", Seq(
          Attribute.NsImport("", "some-namespace") )),
        HighLevelEvent.Whitespace(ep, "\n  "),
        HighLevelEvent.ElementOpen(ep, "", "feature", "with-cdata", Seq(
          Attribute.NsImport("", "with-cdata") )),
        HighLevelEvent.CData(ep, "some-text"),
        HighLevelEvent.ElementClose(ep, "", "feature", "with-cdata"),
        HighLevelEvent.Whitespace(ep, "\n  "),
        HighLevelEvent.ElementOpen(ep, "", "feature", "with-pcdata", Seq(
          Attribute.NsImport("", "with-pcdata") )),
        HighLevelEvent.PCData(ep, "some-text"),
        HighLevelEvent.ElementClose(ep, "", "feature", "with-pcdata"),
        HighLevelEvent.Whitespace(ep, "\n "),
        HighLevelEvent.ElementClose(ep, "streams", "features", "http://streams"),
        HighLevelEvent.Whitespace(ep, "\n"),
        HighLevelEvent.ElementClose(ep, "streams", "stream", "http://streams")
      )

    val p0 = HighLevelParser.empty.withoutPosition.in(input)
    val p1 = checkExpectedEvents(p0)(expectedEvents)

    p1.inputBuffer.mkString should be ("\nSome rubbish goes here...")
  }

  it should "parse #10 (russian text)" in {
    common("<text xmlns='namespace'>text по-русски trentemøller</text>", Seq(
      HighLevelEvent.ElementOpen(ep, "", "text", "namespace", Seq(Attribute.NsImport("", "namespace"))),
      HighLevelEvent.PCData(ep, "text по-русски trentemøller"),
      HighLevelEvent.ElementClose(ep, "", "text", "namespace")
    ))
  }

  it should "parse #11 (xml-entities)" in {
    common("<text xmlns='namespace'>pre&amp;&lt;&gt;&quot;&apos;post</text>", Seq(
      HighLevelEvent.ElementOpen(ep, "", "text", "namespace", Seq(Attribute.NsImport("", "namespace"))),
      HighLevelEvent.PCData(ep, "pre&<>\"'post"),
      HighLevelEvent.ElementClose(ep, "", "text", "namespace")
    ))
  }

  it should "parse #12 (manually registered prefix)" in {
    common("<?xml version='1.0' ?><stream:stream to='c2s.qamain.xmppcs.dev' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'  xml:lang='en' version='1.0'>",
      Seq(
        HighLevelEvent.ProcessingInstrutcion(ep, "xml", "version='1.0' "),
        HighLevelEvent.ElementOpen(ep, "stream", "stream", "http://etherx.jabber.org/streams", Seq(
          Attribute.Unprefixed("to", "c2s.qamain.xmppcs.dev"),
          Attribute.NsImport("", "jabber:client"),
          Attribute.NsImport("stream", "http://etherx.jabber.org/streams"),
          Attribute.Prefixed("xml", "lang", "en"),
          Attribute.Unprefixed("version", "1.0")
        ))
      ), HighLevelParser.empty.withoutPosition.registerPrefix("xml", "__xml__"))
  }


  private def common(input: String, expectedEvents: Seq[HighLevelEvent], p0: HighLevelParser = HighLevelParser.empty.withoutPosition): Unit = {
    val p1 = p0.in(input)
    val p2 = checkExpectedEvents(p1)(expectedEvents)
    ensureParserInputUnderrun(p2)
    ()
  }

  private def checkExpectedEvents(p0: HighLevelParser)(expectedEvents: Seq[HighLevelEvent]): HighLevelParser = {
    expectedEvents.foldLeft(p0) {
      case (pIn, eventExpected) =>
        val (eventActual, pOut) = pIn.out
        eventActual should be (eventExpected)
        pOut
    }
  }


  private def ensureParserInputUnderrun(p: HighLevelParser): HighLevelParser = {
    try {
      val undesiredEvent = p.out
      throw new Exception("Expected input underrun to be thrown. Got event: %s".format(undesiredEvent))
    } catch {
      case pe @ HighLevelParserError.LowLevel(parser,
                  LowLevelParserError.TokError(_,
                    TokenizerError.InputBufferUnderrun(_)))
      =>
        pe.description.nonEmpty should be (true)
        pe.position should be (ep)

        parser
    }

  }

}
