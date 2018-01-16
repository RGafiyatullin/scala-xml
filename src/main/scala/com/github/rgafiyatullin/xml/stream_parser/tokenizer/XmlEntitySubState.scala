package com.github.rgafiyatullin.xml.stream_parser.tokenizer

sealed trait XmlEntitySubState
object XmlEntitySubState {
  val decDigitValue: PartialFunction[Char, Int] = {
    case '0' => 0
    case '1' => 1
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
  }

  val hexDigitValue: PartialFunction[Char, Int] =
    decDigitValue orElse {
      case 'a' => 10
      case 'A' => 10

      case 'b' => 11
      case 'B' => 11

      case 'c' => 12
      case 'C' => 12

      case 'd' => 13
      case 'D' => 13

      case 'e' => 14
      case 'E' => 14

      case 'f' => 15
      case 'F' => 15
    }

  def isDecOrHexDigit(ch: Char): Boolean =
    hexDigitValue.isDefinedAt(ch)

  def decChar(a: Char, b: Char, c: Char, d: Char): Char =
    Seq(d,c,b,a).map(decDigitValue).foldLeft((1, 0)) {
      case ((mult, acc), x) =>
        (mult * 10, acc + mult * x)
    }._2.toChar

  def hexChar(a: Char, b: Char, c: Char, d: Char): Char =
    Seq(d,c,b,a).map(hexDigitValue).foldLeft((1, 0)) {
      case ((mult, acc), x) =>
        (mult * 16, acc + mult * x)
    }._2.toChar

  case object Amp extends XmlEntitySubState

  case object AmpL extends XmlEntitySubState
  case object AmpLT extends XmlEntitySubState

  case object AmpG extends XmlEntitySubState
  case object AmpGT extends XmlEntitySubState

  case object AmpA extends XmlEntitySubState
  case object AmpAM extends XmlEntitySubState
  case object AmpAMP extends XmlEntitySubState

  case object AmpQ extends XmlEntitySubState
  case object AmpQU extends XmlEntitySubState
  case object AmpQUO extends XmlEntitySubState
  case object AmpQUOT extends XmlEntitySubState

  case object AmpAP extends XmlEntitySubState
  case object AmpAPO extends XmlEntitySubState
  case object AmpAPOS extends XmlEntitySubState

  case object AmpPound extends XmlEntitySubState

  final case class AmpPoundD(a: Char) extends XmlEntitySubState
  final case class AmpPoundDD(a: Char, b: Char) extends XmlEntitySubState
  final case class AmpPoundDDD(a: Char, b: Char, c: Char) extends XmlEntitySubState
  final case class AmpPoundDDDD(a: Char, b: Char, c: Char, d: Char) extends XmlEntitySubState

  case object AmpPoundX extends XmlEntitySubState
  final case class AmpPoundXH(a: Char) extends XmlEntitySubState
  final case class AmpPoundXHH(a: Char, b: Char) extends XmlEntitySubState
  final case class AmpPoundXHHH(a: Char, b: Char, c: Char) extends XmlEntitySubState
  final case class AmpPoundXHHHH(a: Char, b: Char, c: Char, d: Char) extends XmlEntitySubState
}
