package com.yuvimasory.cardgames

/* Shoes */
sealed trait Shoe {
  def summary: String
  def draw(n: Int): (Seq[PlayingCard], Shoe)
  def burn(n: Int): Shoe = draw(n)._2
}
class InfiniteShoe() extends Shoe {
  override def draw(n: Int) = {
    val drawnCards: Seq[PlayingCard] =
      (1 to n).map{ i => PlayingCard.next() }.toSeq
    (drawnCards, this)
  }
  override def summary = "an infinite shoe"
}
class FiniteShoe(cards: Seq[PlayingCard], numDecks: Int) extends Shoe {

  override def draw(n: Int) = {
    val (left, right) = cards splitAt n
    (left, new FiniteShoe(right, numDecks))
  }

  override def summary = numDecks match {
    case 1 => "a single deck"
    case n => "%s decks" format n
  }
}
object FiniteShoe {

  def next(numDecks: Int) = new FiniteShoe(
    shuffle {
      (1 to numDecks) flatMap { i => AngloDeck.next.cards }
    },
    numDecks
  )

  private[this] def shuffle(seq: Seq[PlayingCard]): Seq[PlayingCard] = {
    import scala.collection.JavaConverters._
    val jList = new java.util.ArrayList(seq.toList.asJava)
    java.util.Collections shuffle (jList, rand)
    jList.asScala.toSeq
  }
}

/* Decks */
case class AngloDeck private (cards: Seq[PlayingCard]) {
  def size(): Int = cards.length
  def drawCard(): AngloDeck = AngloDeck(cards.tail)
  def remainingCards: Seq[PlayingCard] = cards
}
object AngloDeck {
  val AllSuits: List[Suit] = List(Hearts, Diamonds, Spades, Clubs)
  val AllRanks: List[Rank] = Ace :: King :: Queen :: Jack :: {
    { for (i <- 2 to 10) yield NumericRank(i) }.toList
  }
  val Size = 52

  def next(): AngloDeck = {
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



/* Ranks */
sealed trait Rank {
  def baccaratValue: Int
}
object Rank {
  val AllRanks: Seq[Rank] =
    Ace.AllRanks ++ Face.AllRanks ++ NumericRank.AllRanks
  def next(): Rank = AllRanks(rand nextInt AllRanks.length)
}
case class NumericRank(val rank: Int) extends Rank {
  override def baccaratValue = rank
  override def toString = rank.toString
}
object NumericRank {
  val AllRanks: Seq[Rank] = (2 to 10) map { NumericRank(_) }
}
sealed trait NonNumericRank extends Rank
sealed trait Face extends NonNumericRank {
  override def baccaratValue = 10
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
  override def baccaratValue = 1
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
