package com.yuvimasory.casinosim

import java.io.{ BufferedReader, File, FileReader }

import scala.annotation.tailrec

sealed trait Game {

  /* private */
  private[this] val DecFmt = new java.text.DecimalFormat("###.####")

  /* interface to implement */
  type MyWager <: Wager
  type MyGameRound <: GameRound
  type MyStats <: AggregateStats
  type MyPlayer <: GamePlayer

  val Name: String
  val EmptyStats: MyStats
  val RoundKey: Option[String] = None
  val player: MyPlayer

  trait GameRound {
    def serialize: String
    def outcomes: List[(Wager, Int)]
  }

  trait Wager {
    val Name: String
  }

  trait GamePlayer {
    def play(): MyGameRound
  }

  trait AggregateStats {
    def :+(res: MyGameRound): MyStats
    def summary: String
  }
}

abstract class CardGame(val deckDesc: DeckDescription) extends Game {
  protected[this] def createShoe(): Shoe = deckDesc match {
    case FiniteDecks(n) => FiniteShoe next n
    case InfiniteDecks  => InfiniteShoe next()
  }
}

trait DiceGame extends Game
