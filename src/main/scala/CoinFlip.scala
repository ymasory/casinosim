package com.yuvimasory.casinosim

import scala.annotation.tailrec

class CoinFlip() extends CoinGame() {

  override type MyWager = CoinFlipWager
  override type MyGameRound = CoinFlipRound
  override type MyPlayer = CoinFlipPlayer

  override val Name = "Coin Flip"

  override val player: CoinFlipPlayer = new CoinFlipPlayer()

  class CoinFlipRound(flip: CoinFlipResult) extends GameRound {

    val AllWagers = List(HeadsUp, TailsUp)

    override def serialize = flip match {
      case Heads => "H"
      case Tails => "T"
    }

    override def outcomes = {
      AllWagers.map { wager =>
        val outcome = flip match {
          case Heads => if (wager == HeadsUp) 1 else -1
          case Tails => if (wager == TailsUp) 1 else -1
        }
        (wager, outcome)
      }
    }
  }

  sealed trait CoinFlipWager extends Wager
  case object HeadsUp extends CoinFlipWager {
    override val Name = "Heads"
  }
  case object TailsUp extends CoinFlipWager {
    override val Name = "Tails"
  }

  class CoinFlipPlayer() extends GamePlayer() {
    override def play(): CoinFlipRound = new CoinFlipRound(Coin flip())
  }

  private[this] sealed trait CoinFlipResult
  private[this] case object Heads extends CoinFlipResult
  private[this] case object Tails extends CoinFlipResult
  private[this] object Coin {
    def flip() = if (Rand nextBoolean()) Heads else Tails
  }
}
