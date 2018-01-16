package com.github.rgafiyatullin.xml.stream_parser.tokenizer

import com.github.rgafiyatullin.xml.common.Position
import com.github.rgafiyatullin.xml.stream_parser.tokenizer.State.ProcessChar

import scala.collection.immutable.Queue

sealed trait State {
  def processChar(position: Position): ProcessChar
}

object State {
  type ProcessChar = PartialFunction[Char, (Seq[Token], State)]

  private def isNameStartChar(ch: Char): Boolean =
    ch == ':' ||
    ch == '_' ||
    ch.isLetter

  private def isNameChar(ch: Char): Boolean =
    isNameStartChar(ch) ||
    ch.isDigit ||
    ch == '-' ||
    ch == '.'

  private def isWhitespace(ch: Char): Boolean =
    ch.isWhitespace


  case object Normal extends State {
    override def processChar(position: Position): ProcessChar = {
      case '<' => (Seq(), CornerBracketOpen)
      case '&' => (Seq(), XmlEntity(XmlEntitySubState.Amp))
      case ch if isWhitespace(ch) => (Seq(Token.Whitespace(position, ch)), Normal)
      case ch => (Seq(Token.Character(position, ch)), Normal)
    }
  }

  final case class XmlEntity(s: XmlEntitySubState) extends State {
    import com.github.rgafiyatullin.xml.stream_parser.tokenizer.{XmlEntitySubState => S}

    override def processChar(position: Position): ProcessChar = {
      case 'l' if s == S.Amp => (Seq(), XmlEntity(S.AmpL))
      case 't' if s == S.AmpL => (Seq(), XmlEntity(S.AmpLT))
      case ';' if s == S.AmpLT => (Seq(Token.Character(position, '<')), Normal)

      case 'g' if s == S.Amp => (Seq(), XmlEntity(S.AmpG))
      case 't' if s == S.AmpG => (Seq(), XmlEntity(S.AmpGT))
      case ';' if s == S.AmpGT => (Seq(Token.Character(position, '>')), Normal)

      case 'a' if s == S.Amp => (Seq(), XmlEntity(S.AmpA))
      case 'm' if s == S.AmpA => (Seq(), XmlEntity(S.AmpAM))
      case 'p' if s == S.AmpAM => (Seq(), XmlEntity(S.AmpAMP))
      case ';' if s == S.AmpAMP => (Seq(Token.Character(position, '&')), Normal)

      case 'p' if s == S.AmpA => (Seq(), XmlEntity(S.AmpAP))
      case 'o' if s == S.AmpAP => (Seq(), XmlEntity(S.AmpAPO))
      case 's' if s == S.AmpAPO => (Seq(), XmlEntity(S.AmpAPOS))
      case ';' if s == S.AmpAPOS => (Seq(Token.Character(position, '\'')), Normal)

      case 'q' if s == S.Amp => (Seq(), XmlEntity(S.AmpQ))
      case 'u' if s == S.AmpQ => (Seq(), XmlEntity(S.AmpQU))
      case 'o' if s == S.AmpQU => (Seq(), XmlEntity(S.AmpQUO))
      case 't' if s == S.AmpQUO => (Seq(), XmlEntity(S.AmpQUOT))
      case ';' if s == S.AmpQUOT => (Seq(Token.Character(position, '"')), Normal)

      case '#' if s == S.Amp => (Seq(), XmlEntity(S.AmpPound))
      case 'x' if s == S.AmpPound => (Seq(), XmlEntity(S.AmpPoundX))
      case 'X' if s == S.AmpPound => (Seq(), XmlEntity(S.AmpPoundX))

      case ch if S.isDecOrHexDigit(ch) =>
        s match {
          case S.AmpPound if ch.isDigit =>
            (Seq(), XmlEntity(S.AmpPoundD(ch)))
          case S.AmpPoundX =>
            (Seq(), XmlEntity(S.AmpPoundXH(ch)))

          case S.AmpPoundD(a) if ch.isDigit =>
            (Seq(), XmlEntity(S.AmpPoundDD(a, ch)))
          case S.AmpPoundDD(a, b) if ch.isDigit =>
            (Seq(), XmlEntity(S.AmpPoundDDD(a, b, ch)))
          case S.AmpPoundDDD(a, b, c) if ch.isDigit =>
            (Seq(), XmlEntity(S.AmpPoundDDDD(a, b, c, ch)))

          case S.AmpPoundXH(a) =>
            (Seq(), XmlEntity(S.AmpPoundXHH(a, ch)))
          case S.AmpPoundXHH(a, b) =>
            (Seq(), XmlEntity(S.AmpPoundXHHH(a, b, ch)))
          case S.AmpPoundXHHH(a, b, c) =>
            (Seq(), XmlEntity(S.AmpPoundXHHHH(a, b, c, ch)))
        }

      case ';' =>
        s match  {
          case S.AmpPoundXHHHH(a, b, c, d) =>
            (Seq(Token.Character(position, S.hexChar(a, b, c, d))), Normal)

          case S.AmpPoundDDDD(a, b, c, d) if d.isDigit =>
            (Seq(Token.Character(position, S.decChar(a, b, c, d))), Normal)
        }
    }
  }


  // Met '<'
  case object CornerBracketOpen extends State {
    override def processChar(position: Position): ProcessChar = {
      case '?' => (Seq(Token.PIOpen(position)), InsidePITarget(Queue.empty))
      case '/' => (Seq(Token.LtSlash(position)), InsideTag(Queue.empty))
      case '!' => (Seq(), CommentOrCDataStarting)
      case nsc if isNameStartChar(nsc) => (Seq(Token.Lt(position)), InsideTag(Queue(nsc)))
    }
  }

  // Met '<!'
  case object CommentOrCDataStarting extends State {
    override def processChar(position: Position): ProcessChar = {
      case '[' => (Seq(), StartingCData(StartingCDataSubstate.E))
      case '-' => (Seq(), StartingComment)
    }
  }

  // Met '<!-'
  case object StartingComment extends State {
    override def processChar(position: Position): ProcessChar = {
      case '-' => (Seq(Token.CommentOpen(position)), InsideComment(Queue.empty))
    }
  }

  // Inside of a comment: accumulating text
  final case class InsideComment(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '-' => (Seq(), InsideCommentHyphen(acc))
      case ch => (Seq(), copy(acc = acc.enqueue(ch)))
    }
  }

  // Met '-' inside of a comment
  final case class InsideCommentHyphen(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '-' => (Seq(Token.CommentText(position, acc.mkString)), ClosingComment)
      case other => (Seq(), InsideComment(acc.enqueue('-').enqueue(other)))
    }
  }

  // Met '--' inside of a comment
  case object ClosingComment extends State {
    override def processChar(position: Position): ProcessChar = {
      case '>' => (Seq(Token.CommentClose(position)), Normal)
    }
  }

  final case class InsidePITarget(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case nws if !isWhitespace(nws) =>
        (Seq(), copy(acc = acc.enqueue(nws)))

      case ws if isWhitespace(ws) =>
        (Seq(Token.PITarget(position, acc.mkString)), InsidePIContent(Queue.empty))
    }
  }

  final case class InsidePIContent(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '?' => (Seq(), InsidePIContentQMark(acc))
      case ch => (Seq(), copy(acc = acc.enqueue(ch)))
    }
  }

  final case class InsidePIContentQMark(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '>' => (
        Seq(
          Token.PIContent(position, acc.mkString),
          Token.PIClose(position)),
        Normal)

      case ch =>
        (Seq(), InsidePIContent(acc.enqueue('?').enqueue(ch)))
    }
  }


  final case class StartingCData(sub: StartingCDataSubstate) extends State {
    private val s = StartingCDataSubstate

    override def processChar(position: Position): ProcessChar =
      sub match {
        case s.E => { case 'C' => (Seq(), copy(sub = s.C)) }
        case s.C => { case 'D' => (Seq(), copy(sub = s.CD)) }
        case s.CD => { case 'A' => (Seq(), copy(sub = s.CDA)) }
        case s.CDA => { case 'T' => (Seq(), copy(sub = s.CDAT)) }
        case s.CDAT => { case 'A' => (Seq(), copy(sub = s.CDATA)) }
        case s.CDATA => { case '[' => (Seq(Token.CDataOpen(position)), InsideCData(Queue.empty)) }
      }
  }


  final case class InsideCData(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case ']' => (Seq(), InsideCDataFirstBracket(acc))
      case ch => (Seq(), copy(acc.enqueue(ch)))
    }
  }

  final case class InsideCDataFirstBracket(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case ']' => (Seq(), InsideCDataSecondBracket(acc))
      case ch => (Seq(), InsideCData(acc.enqueue(']').enqueue(ch)))
    }
  }

  final case class InsideCDataSecondBracket(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '>' => (
        Seq(
          Token.CDataContent(position, acc.mkString),
          Token.CDataClose(position)),
        Normal)

      case ']' =>
        (Seq(), copy(acc = acc.enqueue(']')))

      case ch =>
        (Seq(), InsideCData(acc.enqueue(']').enqueue(']').enqueue(ch)))
    }
  }


  final case class InsideTag(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {

      case nsc if acc.isEmpty && isNameStartChar(nsc) =>
        (Seq(), copy(acc = acc.enqueue(nsc)))

      case nc if acc.nonEmpty && isNameChar(nc) =>
        (Seq(), copy(acc = acc.enqueue(nc)))

      case ws if isWhitespace(ws) =>
        (maybeXmlName(position), InsideTag(Queue.empty))


      case '=' =>
        (maybeXmlName(position) ++ Seq(Token.EqSign(position)), InsideTag(Queue.empty))

      case '"' =>
        (maybeXmlName(position), InsideAttributeValue(Queue.empty))

      case '\'' =>
        (maybeXmlName(position), InsideAttributeValueSingleQuoted(Queue.empty))

      case '>' =>
        (maybeXmlName(position) ++ Seq(Token.Gt(position)), Normal)

      case '/' =>
        (maybeXmlName(position), InsideTagTrailingSlash)
    }

    private def maybeXmlName(position: Position): Seq[Token] =
      if (acc.isEmpty) Seq()
      else Seq(Token.XmlName(position, acc.mkString))
  }

  case object InsideTagTrailingSlash extends State {
    override def processChar(position: Position): ProcessChar = {
      case '>' => (Seq(Token.SlashGt(position)), Normal)
    }
  }

  final case class InsideAttributeValue(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '"' =>
        (Seq(Token.AttributeValue(position, acc.mkString)), InsideTag(Queue.empty))

      case ch =>
        (Seq(), copy(acc = acc.enqueue(ch)))
    }
  }

  final case class InsideAttributeValueSingleQuoted(acc: Queue[Char]) extends State {
    override def processChar(position: Position): ProcessChar = {
      case '\'' =>
        (Seq(Token.AttributeValue(position, acc.mkString)), InsideTag(Queue.empty))

      case ch =>
        (Seq(), copy(acc = acc.enqueue(ch)))
    }
  }

}

