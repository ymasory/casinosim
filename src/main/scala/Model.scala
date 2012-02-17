package com.yuvimasory.cardgames

import scala.collection.JavaConverters._

/* Shoes */
case class InfiniteShoe() {
  def deal: PlayingCard = PlayingCard.next()
  def summary = "infinite shoe"
}

sealed trait Cut
case class CutCard(num: Int) extends Cut

/* Decks */
case class AngloDeck private (cards: Seq[PlayingCard]) {
  def sizeWithCut(): Int = cards.length
  def drawCard(): AngloDeck = AngloDeck(cards.tail)
  def shuffle(): AngloDeck = {
    val cardsAsJava = cards.toList.asJava
    java.util.Collections.shuffle(cardsAsJava, rand)
    AngloDeck(cardsAsJava.asScala)
  }
  def remainingCards: Seq[PlayingCard] = cards
}
object AngloDeck {
  val AllSuits: List[Suit] = List(Hearts, Diamonds, Spades, Clubs)
  val AllRanks: List[Rank] = Ace :: King :: Queen :: Jack :: {
    { for (i <- 2 to 10) yield NumericRank(i) }.toList
  }
  val Size = 52

  def next() = nextUnShuffledDeck.shuffle()

  private[this] def nextUnShuffledDeck(): AngloDeck = {
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
case class PlayingCard(suit: Suit, rank: Rank) extends Ordered[PlayingCard] {

  override def toString = "%s-%s" format (rank.toString, suit.toString)

  private def sortingNum = rank match {
    case NumericRank(n) => n
    case Jack           => 11
    case Queen          => 12
    case King           => 13
    case Ace            => 14
  }

  override def compare(that: PlayingCard) = {
    val thisNum = sortingNum
    val thatNum = that.sortingNum
    if (thisNum < thatNum) -1 else if (thisNum > thatNum) 1 else 0
  }
}
object PlayingCard {
  def next(): PlayingCard = PlayingCard(Suit.next(), Rank.next())
}
// case object CutCard extends Card {
//   override def toString = "<CUT CARD>"
// }



/* Ranks */
sealed trait Rank
object Rank {
  val AllRanks: Seq[Rank] =
    Ace.AllRanks ++ Face.AllRanks ++ NumericRank.AllRanks
  def next(): Rank = AllRanks(rand nextInt AllRanks.length)
}
case class NumericRank(val rank: Int) extends Rank {
  override def toString = rank.toString
}
object NumericRank {
  val AllRanks: Seq[Rank] = (2 to 10) map { NumericRank(_) }
}
sealed trait NonNumericRank extends Rank
sealed trait Face extends NonNumericRank {
  override def toString = this match {
    case Jack  => "J"
    case Queen => "Q"
    case King  => "K"
  }
}
object Face {
  val AllRanks = Seq(Jack, Queen, King)
}
case object Jack extends Face
case object Queen extends Face
case object King extends Face
case object Ace extends NonNumericRank {
  override def toString = "A"
  val AllRanks = Seq(Ace)
}



/* Suits */
object Suit {
  val AllSuits: Seq[Suit] = Seq(Hearts, Clubs, Spades, Diamonds)
  def next(): Suit = AllSuits(rand nextInt AllSuits.length)
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
