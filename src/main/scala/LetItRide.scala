package com.yuvimasory.casinosim

class LetItRide() extends DiceGame() {

  override type MyWager = LiRWager
  override type MyGameRound = LiRRound
  override type MyPlayer = LiRPlayer

  override val Name = "Let it Ride"

  override val player = new LiRPlayer()

  sealed trait LiRWager extends Wager
  case object PokerWager extends Wager {
    override val Name = "Main Let it Ride Wager"
  }

  class LiRRound() extends GameRound {
    override def serialize = null
    override def outcomes = null
  }
  
  class LiRPlayer() extends GamePlayer() {

    override def play(): LiRRound = null
  }

  /* internal */
  // case class Hand(cards: List[Card]) {

  //   private[this] lazy val ranks: List[Int] = cards.map(_.ranks.acesHigh)
  //   private[this] lazy val suits: List[Suit] = cards.map(_.suit)

  //   def isRoyal = isFlush && isStraight && (hiCard.rank == Ace)
  //   def isFlush = suits.distinct.length == 1
  //   def isStraight = {
  //     val ranks = cards.map(_.aceHigh)
  //     val (min, max) = (ranks.min, ranks.max)
  //     (max - min) == 4
  //   }
  //   def fourKind = {
  //     (cards.length >= 4) && ranks.distinct <
  //   }
  //   def hiCard: Card = null
  //   def fourKind = 
  // }
}
