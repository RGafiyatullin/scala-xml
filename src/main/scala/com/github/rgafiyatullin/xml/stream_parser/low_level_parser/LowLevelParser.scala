package com.github.rgafiyatullin.xml.stream_parser.low_level_parser

import com.github.rgafiyatullin.xml.common.LowLevelEvent
import com.github.rgafiyatullin.xml.stream_parser._
import com.github.rgafiyatullin.xml.stream_parser.tokenizer.{Token, Tokenizer, TokenizerError}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try


object LowLevelParser {
  def empty: LowLevelParser = LowLevelParser(
    Tokenizer.empty,
    Queue.empty,
    LowLevelState.Initial)
}

case class LowLevelParser(tokenizer: Tokenizer, output: Queue[LowLevelEvent], state: LowLevelState) {
  def inputBuffer: Queue[Char] = tokenizer.inputBuffer

  def in(string: String): LowLevelParser = copy(tokenizer = tokenizer.in(string))
  def in(char: Char): LowLevelParser = copy(tokenizer = tokenizer.in(char))

  def out: (LowLevelEvent, LowLevelParser) =
    if (output.isEmpty)
      outProcessingInputLoop
    else {
      val (event, nextOutput) = output.dequeue
      (event, copy(output = nextOutput))
    }

  def withoutPosition: LowLevelParser =
    copy(tokenizer = tokenizer.withoutPosition)

  @tailrec
  private def outProcessingInputLoop: (LowLevelEvent, LowLevelParser) = {
    val (token, nextTokenizer) =
      Try(tokenizer.out)
        .recover({
          case tokError: TokenizerError =>
            throw LowLevelParserError.TokError(this, tokError)
        }).get

    val (events, nextState) = state.processToken
      .applyOrElse(token, throwUnexpectedToken(state))
    val nextParser = copy(tokenizer = nextTokenizer, state = nextState)
    if (events.isEmpty)
      nextParser.outProcessingInputLoop
    else {
      val eventsHead = events.head
      val eventsTail = events.tail
      val nextOutput = eventsTail.foldLeft(nextParser.output) { _.enqueue(_) }
      (eventsHead, nextParser.copy(output = nextOutput))
    }
  }

  private def throwUnexpectedToken(state: LowLevelState)(token: Token): Nothing =
    throw LowLevelParserError.UnexpectedToken(token, state)

}

