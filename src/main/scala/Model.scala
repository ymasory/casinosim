package com.yuvimasory.casinosim

import java.security.SecureRandom

object Rand extends SecureRandom

/* Dice */
object Die {
  def roll(): Byte = (Rand.nextInt(6) + 1).toByte
}

/* Shoes */
sealed trait Shoe {
  def draw1(): (Card, Shoe)
  def draw2(): (Card, Card, Shoe)
  def draw3(): (Card, Card, Card, Shoe)
  def draw(n: Int): (List[Card], Shoe)
}

class InfiniteShoe private () extends Shoe {
  override def draw1() = (Card.next(), this)
  override def draw2() = (Card.next(), Card.next(), this)
  override def draw3() = (Card.next(), Card.next(), Card.next(), this)
  override def draw(n: Int) = {
    val drawnCards: List[Card] =
      (1 to n).map{ _ => Card.next() }.toList
    (drawnCards, this)
  }
}
object InfiniteShoe {
  def next() = new InfiniteShoe
}

class FiniteShoe private (cards: List[Card]) extends Shoe {
  override def draw1() = {
    val (List(card), shoe) = draw(1)
    (card, shoe)
  }
  override def draw2() = {
    val (List(card1, card2), shoe) = draw(2)
    (card1, card2, shoe)
  }
  override def draw3() = {
    val (List(card1, card2, card3), shoe) = draw(3)
    (card1, card2, card3, shoe)
  }
  override def draw(n: Int) = {
    val (left, right) = cards splitAt n
    (left, new FiniteShoe(right))
  }
}

object FiniteShoe {

  def next(numDecks: Int) = {
    val cards = (1 to numDecks).flatMap { _ => AngloDeck.next.cards }.toList
    new FiniteShoe(cards)
  }
}

/* Decks */
sealed trait DeckDescription {
  override def toString = this match {
    case FiniteDecks(n) => n.toString + " decks"
    case InfiniteDecks  => "an infinite number of decks"
  }
}
object DeckDescription {
  def fromInt(n: Int) = if (n <= 0) InfiniteDecks else FiniteDecks(n)
}
case class FiniteDecks(n: Int) extends DeckDescription
case object InfiniteDecks extends DeckDescription

case class AngloDeck private (val cards: List[Card])
object AngloDeck {
  val AllSuits: List[Suit] = List(Hearts, Diamonds, Spades, Clubs)
  val AllRanks: List[Rank] = Ace :: King :: Queen :: Jack :: {
    { for (i <- 2 to 10) yield NumericRank(i) }.toList
  }

  def next(): AngloDeck = {
    val cards = {
      for {
        suit <- AllSuits
        rank <- AllRanks } yield {
          Card(suit, rank)
        }
    }.toList
    AngloDeck(shuffle(cards))
  }

  private[this] def shuffle(cards: List[Card]): List[Card] = {
    import scala.collection.JavaConverters._
    val jList = new java.util.ArrayList(cards.asJava)
    java.util.Collections shuffle (jList, Rand)
    jList.asScala.toList
  }
}

/* Cards */
case class Card(suit: Suit, rank: Rank) {
  def bacc = rank.bacc
  def war = rank.war
  override def toString = rank.toString + suit.toString
}
object Card {
  def next(): Card = Card(Suit.next(), Rank.next())
}

/* Ranks */
sealed trait Rank {
  def bacc: Int
  def war: Int
}
object Rank {
  val AllRanks: Vector[Rank] =
    Vector.empty ++ { Ace :: Face.AllRanks ++ NumericRank.AllRanks }
  def next(): Rank = AllRanks(Rand nextInt AllRanks.length)
}
case class NumericRank(val rank: Int) extends Rank {
  override def bacc = rank % 10
  override def war = rank
  override def toString = rank.toString
}
object NumericRank {
  val AllRanks: Vector[Rank] = Vector.empty ++ { (2 to 10) map { NumericRank(_) } }
}
sealed trait NonNumericRank extends Rank
sealed trait Face extends NonNumericRank {
  override def bacc = 0
  override def war = this match {
    case Jack  => 10
    case Queen => 11
    case King  => 12
  }
  override def toString = this match {
    case Jack  => "J"
    case Queen => "Q"
    case King  => "K"
  }
}
object Face {
  val AllRanks = List(Jack, Queen, King)
}
case object Jack extends Face
case object Queen extends Face
case object King extends Face
case object Ace extends NonNumericRank {
  override def bacc = 1
  override def war = 11
  override def toString = "A"
}



/* Suits */
object Suit {
  val AllSuits = Vector(Hearts, Clubs, Spades, Diamonds)
  def next(): Suit = AllSuits(Rand nextInt AllSuits.length)
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
