package com.github.rgafiyatullin.xml.stream_parser

import com.github.rgafiyatullin.xml.common.Position
import com.github.rgafiyatullin.xml.stream_parser.tokenizer.{Token, Tokenizer, TokenizerError}
import org.scalatest.{FlatSpec, Matchers}

class TokenizerSpec extends FlatSpec with Matchers {
  val ep = Position.withoutPosition

  "An empty tokenizer" should "throw InputBufferUnderrun exception" in {
    intercept[TokenizerError.InputBufferUnderrun] {
      Tokenizer.empty.out
    }
    ()
  }

  "A tokenizer" should "return a copy with an ephemeral position from withoutPosition-method" in {
    val p = Tokenizer.empty.withoutPosition.position
    p.absolute should be (-1)
    p.line should be (-1)
    p.column should be (-1)
  }

  it should "tokenize comment" in {
    common("<!--text goes-here-->", Seq(
      Token.CommentOpen(ep),
      Token.CommentText(ep, "text goes-here"),
      Token.CommentClose(ep)))
  }

  it should "tokenize PI no qmarks" in {
    piCommon(
      "<?xml-stylesheet type=\"text/xsl\" href=\"style.xsl\" ?>",
      "xml-stylesheet",
      "type=\"text/xsl\" href=\"style.xsl\" ")
  }

  it should "tokenize PI with qmarks in content" in {
    piCommon(
      "<?xml-stylesheet type=\"text/xsl\" href=\"style.xsl\" q-mark-containing=\"???\" ?>",
      "xml-stylesheet",
      "type=\"text/xsl\" href=\"style.xsl\" q-mark-containing=\"???\" ")
  }

  it should "tokenize CDATA no brackets" in {
    cdataCommon("<![CDATA[simple]]>", "simple")
  }

  it should "tokenize CDATA with one bracket in the middle" in {
    cdataCommon("<![CDATA[one]bracket]]>", "one]bracket")
  }

  it should "tokenize CDATA with two brackets in a row in the middle" in {
    cdataCommon("<![CDATA[two]]brackets]]>", "two]]brackets")
  }

  it should "tokenize CDATA with three brackets in a row in the middle" in {
    cdataCommon("<![CDATA[three]]]brackets]]>", "three]]]brackets")
  }

  it should "tokenize CDATA with one extra bracket in the end" in {
    cdataCommon("<![CDATA[one extra bracket]]]>", "one extra bracket]")
  }

  it should "tokenize CDATA with two extra brackets in the end" in {
    cdataCommon("<![CDATA[two extra brackets]]]]>", "two extra brackets]]")
  }

  it should "tokenize CDATA with three extra brackets in the end" in {
    cdataCommon("<![CDATA[three extra brackets]]]]]>", "three extra brackets]]]")
  }

  it should "tokenize CDATA with something similar to xml-entities" in {
    common("<![CDATA[&amp;]]>", Seq(
      Token.CDataOpen(ep),
      Token.CDataContent(ep, "&amp;"),
      Token.CDataClose(ep)))
  }

  it should "tokenize #1 (open-tag)" in {
    common("<open>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.Gt(ep)))
  }

  it should "tokenize #2 (close-tag)" in {
    common("</close>", Seq(
      Token.LtSlash(ep),
      Token.XmlName(ep, "close"),
      Token.Gt(ep)))
  }

  it should "tokenize #3 (self-closing-tag)" in {
    common("<self-closing/>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "self-closing"),
      Token.SlashGt(ep)))
  }

  it should "tokenize #4 (open-tag with trailing whitespace)" in {
    common("<open >", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.Gt(ep)))
  }

  it should "tokenize #5 (close-tag with trailing whitespace)" in {
    common("</close >", Seq(
      Token.LtSlash(ep),
      Token.XmlName(ep, "close"),
      Token.Gt(ep)))
  }

  it should "tokenize #6 (self-closing-tag with trailing whitespace)" in {
    common("<self-closing />", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "self-closing"),
      Token.SlashGt(ep)))
  }

  it should "tokenize #7 (open-tag with an extra name)" in {
    common("<open name>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.XmlName(ep, "name"),
      Token.Gt(ep)))
  }

  it should "tokenize #7 (self-closing-tag with an extra name)" in {
    common("<open name/>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.XmlName(ep, "name"),
      Token.SlashGt(ep)))
  }

  it should "tokenize #8 (open-tag with attributes)" in {
    common("<open attribute-one=\"111\" attribute-two=\"222\">", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.XmlName(ep, "attribute-one"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "111"),
      Token.XmlName(ep, "attribute-two"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "222"),
      Token.Gt(ep)
    ))
  }

  it should "tokenize #8 (self-closing-tag with attributes)" in {
    common("<open attribute-one=\"111\" attribute-two=\"222\"/>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.XmlName(ep, "attribute-one"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "111"),
      Token.XmlName(ep, "attribute-two"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "222"),
      Token.SlashGt(ep)
    ))
  }

  it should "tokenize #9 (open-tag with attributes with single-quotes)" in {
    common("<open attribute-one='111' attribute-two='222'>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.XmlName(ep, "attribute-one"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "111"),
      Token.XmlName(ep, "attribute-two"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "222"),
      Token.Gt(ep)
    ))
  }

  it should "tokenize #10 (self-closing-tag with attributes with single-quotes)" in {
    common("<open attribute-one='111' attribute-two='222'/>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "open"),
      Token.XmlName(ep, "attribute-one"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "111"),
      Token.XmlName(ep, "attribute-two"),
      Token.EqSign(ep),
      Token.AttributeValue(ep, "222"),
      Token.SlashGt(ep)
    ))
  }

  it should "tokenize #11 (pcdata)" in {
    common("<text>a\t \r\nb</text>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "text"),
      Token.Gt(ep),
      Token.Character(ep, 'a'),
      Token.Whitespace(ep, '\t'),
      Token.Whitespace(ep, ' '),
      Token.Whitespace(ep, '\r'),
      Token.Whitespace(ep, '\n'),
      Token.Character(ep, 'b'),
      Token.LtSlash(ep),
      Token.XmlName(ep, "text"),
      Token.Gt(ep)
    ))
  }

  it should "tokenize #12 (pcdata with xml-entities)" in {
    common("<text>a&amp;&lt;&gt;&quot;&apos;b</text>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "text"),
      Token.Gt(ep),
      Token.Character(ep, 'a'),
      Token.Character(ep, '&'),
      Token.Character(ep, '<'),
      Token.Character(ep, '>'),
      Token.Character(ep, '"'),
      Token.Character(ep, '\''),
      Token.Character(ep, 'b'),
      Token.LtSlash(ep),
      Token.XmlName(ep, "text"),
      Token.Gt(ep)
    ))
  }

  it should "tokenize #13 (pcdata with xml-entities)" in {
    common("<text>a&amp;&lt;&gt;&quot;&apos;&#x2229;&#8745;b</text>", Seq(
      Token.Lt(ep),
      Token.XmlName(ep, "text"),
      Token.Gt(ep),
      Token.Character(ep, 'a'),
      Token.Character(ep, '&'),
      Token.Character(ep, '<'),
      Token.Character(ep, '>'),
      Token.Character(ep, '"'),
      Token.Character(ep, '\''),
      Token.Character(ep, '∩'),
      Token.Character(ep, '∩'),
      Token.Character(ep, 'b'),
      Token.LtSlash(ep),
      Token.XmlName(ep, "text"),
      Token.Gt(ep)
    ))
  }

  private def common(in: String, tokensExpected: Seq[Token]): Unit = {
    val tz0 = Tokenizer.empty.withoutPosition.in(in)
    val tz1 = tokensExpected.foldLeft(tz0) {
      case (tzIn, tokenExpected) =>
        val (tokenActual, tzOut) = tzIn.out
        tokenActual should be (tokenExpected)
        tzOut
    }
    intercept[TokenizerError.InputBufferUnderrun] {
      tz1.out
    }
    ()
  }

  private def cdataCommon(cdata: String, cdataContent: String): Unit = {
    common(cdata, Seq(
      Token.CDataOpen(ep),
      Token.CDataContent(ep, cdataContent),
      Token.CDataClose(ep)))
  }

  private def piCommon(pi: String, target: String, content: String): Unit = {
    common(pi, Seq(
      Token.PIOpen(ep),
      Token.PITarget(ep, target),
      Token.PIContent(ep, content),
      Token.PIClose(ep)
    ))
  }
}
