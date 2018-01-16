package com.github.rgafiyatullin.xml.stream_parser.high_level_parser

import com.github.rgafiyatullin.xml.common.{HighLevelEvent, LowLevelEvent, Position}
import com.github.rgafiyatullin.xml.stream_parser.low_level_parser.{LowLevelParser, LowLevelParserError}

import scala.collection.immutable.Queue
import scala.util.Try

object HighLevelParser {
  def empty: HighLevelParser = HighLevelParser(
      LowLevelParser.empty,
      Queue.empty,
      HighLevelState.initialState)
}

case class HighLevelParser(llParser: LowLevelParser, output: Queue[HighLevelEvent], state: HighLevelState) {
  def registerPrefix(prefix: String, namespace: String): HighLevelParser =
    copy(state = state.withNsImportCtx { nsImports => nsImports.add(Position.withoutPosition, prefix, namespace) })

  def withoutPosition: HighLevelParser =
    copy(llParser = llParser.withoutPosition)

  def inputBuffer: Queue[Char] = llParser.inputBuffer

  def in(char: Char): HighLevelParser =
    copy(llParser = llParser.in(char))

  def in(string: String): HighLevelParser =
    copy(llParser = llParser.in(string))

  def out: (HighLevelEvent, HighLevelParser) =
    if (output.isEmpty)
      outProcessingInputLoop
    else {
      val (event, nextOutput) = output.dequeue
      (event, copy(output = nextOutput))
    }

  def outProcessingInputLoop: (HighLevelEvent, HighLevelParser) = {
    val (nextLLEvent, nextLLParser) = Try(llParser.out)
      .recover {
        case llError: LowLevelParserError =>
          throw HighLevelParserError.LowLevel(this, llError)
      }.get
    val (events, nextState) = state.processLowLevelEvent
      .applyOrElse(nextLLEvent, throwUnexpectedLLEvent(state))
    val nextParser = copy(llParser = nextLLParser, state = nextState)

    if (events.isEmpty)
      nextParser.outProcessingInputLoop
    else {
      val eventsHead = events.head
      val eventsTail = events.tail
      val nextOutput = eventsTail.foldLeft(nextParser.output) { _.enqueue(_) }
      (eventsHead, nextParser.copy(output = nextOutput))
    }

  }

  def throwUnexpectedLLEvent(s: HighLevelState)(llEvent: LowLevelEvent): Nothing =
    throw HighLevelParserError.UnexpectedLLEvent(state, llEvent)
}
