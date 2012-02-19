package com.yuvimasory.casinosim

class War(deckDesc: DeckDescription) extends CardGame(deckDesc) {

  override val name: String = "Casino War"
  override val key = Some(
    "player1 dealer1  burn1 burn2 burn3  player2 dealer2"
  )

  override def play(): WarRound = {
    val (p1, d1, postDrawShoe) = createShoe().draw2()
    val outcome =
      if (p1.war != d1.war) SimpleOutcome(p1, d1)
      else {
        val (b1, b2, b3, postBurnShoe) = postDrawShoe draw3()
        val (p2, d2, _) = postBurnShoe draw2()
        GoToWarOutcome((p1, p2), (d1, d2), (b1, b2, b3))
      }
    WarRound(outcome)
  }

  case class WarRound(outcome: WarOutcome) extends GameRound {
    override val repr = {
      outcome match {
        case SimpleOutcome(p, d) => "%s %s" format (p, d)
        case GoToWarOutcome((p1, p2), (d1, d2), (b1, b2, b3)) =>
          "%s %s  %s %s %s  %s %s" format (p1, d1, b1, b2, b3, p2, d2)
      }
    }
  }

  sealed trait WarOutcome
  case class SimpleOutcome(
    val pCard: Card,
    val dCard: Card
  ) extends WarOutcome
  case class GoToWarOutcome(
    val pCards: Pair[Card, Card],
    val dCards: Pair[Card, Card],
    val burns: Triple[Card, Card, Card]
  ) extends WarOutcome
}
