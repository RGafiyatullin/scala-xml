package com.github.rgafiyatullin.xml.stream_parser.tokenizer

sealed trait StartingCDataSubstate

object StartingCDataSubstate {
  case object E extends StartingCDataSubstate
  case object C extends StartingCDataSubstate
  case object CD extends StartingCDataSubstate
  case object CDA extends StartingCDataSubstate
  case object CDAT extends StartingCDataSubstate
  case object CDATA extends StartingCDataSubstate
}

