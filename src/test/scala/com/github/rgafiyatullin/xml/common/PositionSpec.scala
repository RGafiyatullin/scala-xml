package com.github.rgafiyatullin.xml.common

import org.scalatest.{FlatSpec, Matchers}

class PositionSpec extends FlatSpec with Matchers {
  "An ephemeral position" should "return -1 for every property" in {
    val p = Position.withoutPosition
    p.absolute should be (-1)
    p.line should be (-1)
    p.column should be (-1)
    p.isEmpty should be (true)
    p.isDefined should be (false)
  }

  it should "return same position upon bumbing" in {
    val p0 = Position.withoutPosition
    val p1 = p0.bump('a')
    val p2 = p1.bump('\n')
    p0 should be (p1)
    p1 should be (p2)
    p0.isDefined should be (false)
    p0.isEmpty should be (true)
    p1.isDefined should be (false)
    p1.isEmpty should be (true)
    p2.isDefined should be (false)
    p2.isEmpty should be (true)
  }

  "A position" should "increment 'absolute' upon bumbing" in {
    val p0 = Position.initial
    val p1 = p0.bump('a')
    val p2 = p1.bump('\n')

    (p0.absolute + 1) should be (p1.absolute)
    (p1.absolute + 1) should be (p2.absolute)
    p0.isDefined should be (true)
    p0.isEmpty should be (false)
    p1.isDefined should be (true)
    p1.isEmpty should be (false)
    p2.isDefined should be (true)
    p2.isEmpty should be (false)
  }

  it should "increment line number upon bumping with a newline" in {
    val p0 = Position.initial
    val p1 = p0.bump('a')
    val p2 = p1.bump('\n')

    p0.line should be (p1.line)
    (p1.line + 1) should be (p2.line)
    p0.isDefined should be (true)
    p0.isEmpty should be (false)
    p1.isDefined should be (true)
    p1.isEmpty should be (false)
    p2.isDefined should be (true)
    p2.isEmpty should be (false)
  }

  it should "increment column number upon bumping with non-newline and reset column upon newline" in {
    val p0 = Position.initial
    val p1 = p0.bump('a')
    val p2 = p1.bump('\n')

    (p0.column + 1) should be (p1.column)
    p2.column should be (0)
    p0.isDefined should be (true)
    p0.isEmpty should be (false)
    p1.isDefined should be (true)
    p1.isEmpty should be (false)
    p2.isDefined should be (true)
    p2.isEmpty should be (false)
  }
}
