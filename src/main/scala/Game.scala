package com.yuvimasory.casinosim

sealed trait Game {
  val name: String
  val key: Option[String] = None
  def play(): GameRound
  trait GameRound {
    def repr: String
  }
}

abstract class CardGame(val deckDesc: DeckDescription) extends Game {
  protected[this] def createShoe(): Shoe = deckDesc match {
    case FiniteDecks(n) => FiniteShoe next n
    case InfiniteDecks  => InfiniteShoe next()
  }
}

trait DiceGame extends Game
