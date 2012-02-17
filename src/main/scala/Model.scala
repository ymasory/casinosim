package com.yuvimasory.cardgames

import scala.util.Random

/* Shoes */
trait Shoe[A <: Shoe[_]] {
  def peek: Card
  def deal: (Card, A)
  def discard: Seq[PlayingCard]
  def needsShuffle: Boolean
}
case class InfiniteShoe() extends Shoe[InfiniteShoe] {
  override def peek: Card = null
  override def deal: (Card, InfiniteShoe) = (null, null)
  override def discard: Seq[PlayingCard] = null
  override def needsShuffle: Boolean = true
}
object InfiniteShoe {
  def nextShoe(): InfiniteShoe = InfiniteShoe()
}

case class PhysicalShoe private (decks: Seq[AngloDeck], numCardsToCut: Int)
  extends Shoe[PhysicalShoe] {

  private[this] val remainingCards: Seq[Card] = {
    val playingCards = decks.map { _.remainingCards }.flatten
    val splitLoc = playingCards.length - numCardsToCut
    val (left, right) = playingCards.splitAt(splitLoc)
    (left :+ CutCard) ++ right
  }

  override def peek: Card = null
  override def deal: (Card, PhysicalShoe) = (null, null)
  override def discard: Seq[PlayingCard] = null
  override def needsShuffle: Boolean = false
}
object PhysicalShoe {
  def nextShoe(numDecks: Int, numCardsToCut: Int): PhysicalShoe = {
    val decks = (0 until numDecks).map(i => AngloDeck.nextShuffledDeck()).toSeq
    PhysicalShoe(decks, numCardsToCut)
  }
}




/* Decks */
case class AngloDeck private (cards: Seq[PlayingCard]) {
  def sizeWithCut(): Int = cards.length
  def drawCard(): AngloDeck = AngloDeck(cards.tail)
  def shuffle(): AngloDeck = AngloDeck(Random shuffle cards)
  def remainingCards: Seq[PlayingCard] = cards
}
object AngloDeck {
  val AllSuits: List[Suit] = List(Hearts, Diamonds, Spades, Clubs)
  val AllRanks: List[Rank] = Ace :: King :: Queen :: Jack :: {
    { for (i <- 2 to 10) yield NumericRank(i) }.toList
  }

  def nextShuffledDeck() = nextUnShuffledDeck.shuffle()

  def nextUnShuffledDeck(): AngloDeck = {
    val cards = {
      for {
        suit <- AllSuits
        rank <- AllRanks } yield {
          PlayingCard(suit, rank)
        }
    }.toSeq
    AngloDeck(cards)
  }
}



/* Cards */
sealed trait Card
case class PlayingCard(suit: Suit, rank: Rank) extends Card {
  override def toString = "%s-%s" format (rank.toString, suit.toString)
}
case object CutCard extends Card {
  override def toString = "<CUT CARD>"
}



/* Ranks and Suits */
sealed trait Rank
case class NumericRank(val rank: Int) extends Rank {
  override def toString = rank.toString
}
sealed trait NonNumericRank extends Rank
sealed trait Face extends NonNumericRank {
  override def toString = this match {
    case Jack  => "J"
    case Queen => "Q"
    case King  => "K"
  }
}
case object Jack extends Face
case object Queen extends Face
case object King extends Face
case object Ace extends NonNumericRank {
  override def toString = "A"
}
sealed trait Suit {
  override def toString = this match {
    case Hearts   => "H"  //"♡"
    case Clubs    => "C"  //"♧"
    case Spades   => "S"  //"♤"
    case Diamonds => "D"  //"♢"
  }
  def red: Boolean = this match {
    case Hearts | Diamonds => true
    case Spades | Clubs    => false
  }
  def black = red == false
}
case object Hearts extends Suit
case object Clubs extends Suit
case object Spades extends Suit
case object Diamonds extends Suit
