package com.yuvimasory.casinosim

class War(deckDesc: DeckDescription) extends CardGame(deckDesc) {

  override type MyWager = WarWager
  override type MyGameRound = WarRound
  override type MyPlayer = WarPlayer

  override val Name: String = "Casino War"
  override val RoundKey = Some(
    "player1 dealer1 - burn1 burn2 burn3 - player2 dealer2"
  )
  override val player: WarPlayer = new WarPlayer()

  case class WarRound(res: WarResult) extends GameRound {
    override def serialize = res match {
      case SimpleResult(p, d) => "%s - %s" format (p, d)
      case GoToWarResult((p1, p2), (d1, d2), (b1, b2, b3)) =>
        "%s %s - %s %s %s - %s %s" format (p1, d1, b1, b2, b3, p2, d2)
    }
    override def outcomes: List[(WarWager, Int)] = List((MainWager, res.outcome))
  }

  sealed trait WarWager extends Wager
  case object MainWager extends WarWager {
    override val Name = "Main War Wager"
  }

  class WarPlayer() extends GamePlayer() {
    override def play(): WarRound = {
      val (p1, d1, postDrawShoe) = createShoe().draw2()
      val outcome =
        if (p1.aceHigh != d1.aceHigh) SimpleResult(p1, d1)
        else {
          val (b1, b2, b3, postBurnShoe) = postDrawShoe draw3()
          val (p2, d2, _) = postBurnShoe draw2()
          GoToWarResult((p1, p2), (d1, d2), (b1, b2, b3))
        }
      WarRound(outcome)
    }
  }

  sealed trait WarResult {
    def outcome: Int
  }
  case class SimpleResult(
    val pCard: Card,
    val dCard: Card
  ) extends WarResult {
    override def outcome = if (pCard.aceHigh > dCard.aceHigh) 1 else -1
  }
  case class GoToWarResult(
    val pCards: Pair[Card, Card],
    val dCards: Pair[Card, Card],
    val burns: Triple[Card, Card, Card]
  ) extends WarResult {
    override def outcome = if (dCards._2.aceHigh > pCards._2.aceHigh) -2 else 1
  }
}
