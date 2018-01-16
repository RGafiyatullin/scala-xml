package com.github.rgafiyatullin.xml.stream_parser.tokenizer

import com.github.rgafiyatullin.xml.common.Position

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Tokenizer {
  def empty: Tokenizer = Tokenizer(
    Position.initial,
    State.Normal,
    Queue.empty,
    Queue.empty)
}

case class Tokenizer(
  position: Position,
  state: State,
  input: Queue[(Char, Position)],
  output: Queue[Token])
{
  def inputBuffer: Queue[Char] =
    input.map(_._1)

  def withoutPosition: Tokenizer =
    copy(position = Position.withoutPosition)

  def in(ch: Char): Tokenizer = {
    val pair = (ch, position)

    val nextPosition = position.bump(ch)

    copy(position = nextPosition, input = input.enqueue(pair))
  }

  def in(s: String): Tokenizer = {
    val chars = s.toCharArray
    chars.foldLeft(this){
      case (tokenizer, ch) =>
        tokenizer.in(ch)
    }
  }

  @throws(classOf[TokenizerError])
  def out: (Token, Tokenizer) =
    if (output.isEmpty)
      outProcessInputLoop
    else {
      val (token, nextOutput) = output.dequeue
      (token, copy(output = nextOutput))
    }



  @tailrec
  private def outProcessInputLoop: (Token, Tokenizer) =
    input.dequeueOption match {
      case None =>
        throw inputBufferUnderrunException

      case Some(((ch, pos), nextInput)) =>
        val (tokens, nextState) = state.processChar(pos)
          .applyOrElse(ch, throwUnexpectedCharAtPos(pos, state))

        val nextTokenizer = copy(state = nextState, input = nextInput)

        if (tokens.isEmpty)
          nextTokenizer.outProcessInputLoop
        else {
          val token = tokens.head
          val tokensLeft = tokens.tail
          val nextOutput = tokensLeft.foldLeft(nextTokenizer.output) { _.enqueue(_) }
          (token, nextTokenizer.copy(output = nextOutput))
        }
    }

  private def inputBufferUnderrunException: Throwable =
    TokenizerError.InputBufferUnderrun(position)

  private def throwUnexpectedCharAtPos(pos: Position, state: State)(ch: Char): Nothing =
    throw TokenizerError.UnexpectedChar(pos, ch, state)

}

