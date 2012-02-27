package com.yuvimasory.casinosim

class Baccarat(desc: DeckDescription) extends CardGame(desc) {

  override type MyWager = BaccWager
  override type MyGameRound = BaccRound
  override type MyPlayer = BaccPlayer

  override val Name = "Baccarat (Punto Banco)"
  override val RoundKey = Some(
    "player1 player2 [player3] - banker1 banker2 [banker3]"
  )
  override val player: BaccPlayer = new BaccPlayer()

  case class BaccRound(res: BaccResult) extends GameRound {
    override def serialize = "%s - %s" format (res.pHand, res.bHand)
    override def outcomes: List[(BaccWager, Double)] = List (
      (
        PlayerWager,
        if (res.tie) 0D else if (res.playerWins) 1D else -1D
      ),
      (
        BankerWager,
        if (res.tie) 0D else if (res.playerWins) 0.95D else -1D
      ),
      (
        TieWager, 
        if (res.tie) 8D else if (res.playerWins) -1D else -1D
      )
    )
  }

  sealed trait BaccWager extends Wager
  case object PlayerWager extends BaccWager {
    override val Name = "Player"
  }
  case object BankerWager extends BaccWager {
    override val Name = "Banker"
  }
  case object TieWager extends BaccWager {
    override val Name = "Tie"
  }

  class BaccPlayer() extends GamePlayer { 
    override def play(): BaccRound = {
      val shoe = createShoe
      val (List(p1, p2, b1, b2), postDrawShoe) = shoe draw 4
      val ph1 = BaccHand(Vector(p1, p2))
      val bh1 = BaccHand(Vector(b1, b2))
      val (phFinal, bhFinal) = 
        if (ph1.natural || bh1.natural) (ph1, bh1)
        else {
          if (ph1.pat) {
            val phFinal = ph1
            val bhFinal =
              if (bh1.pat) bh1
              else {
                val (b3, _) = postDrawShoe draw1()
                bh1 :+ b3
              }
            (phFinal, bhFinal)
          }
          else {
            val (p3, postP3Shoe) = postDrawShoe draw1()
            val phFinal = ph1 :+ p3
            val bhFinal = {
              val (b3, _) = postP3Shoe draw1()
              val p3v = p3.bacc
              def drawLowerThan(n: Int) = if (p3v < n) bh1 :+ b3 else bh1
              if (p3v == 2 || p3v == 3) drawLowerThan(5)
              else if (p3v == 4 || p3v == 5) drawLowerThan(6)
              else if (p3v == 6 || p3v == 7) drawLowerThan(7)
              else if (p3v == 8) drawLowerThan(3)
              else drawLowerThan(4)
            }
            (phFinal, bhFinal)
          }
        }
      BaccRound(BaccResult(phFinal, bhFinal))
    }
  }

  case class BaccResult(val pHand: BaccHand, val bHand: BaccHand) {
    def tie = pHand.sum == bHand.sum
    def playerWins = pHand.sum > bHand.sum
    def bankerWins = pHand.sum < bHand.sum
  }

  case class BaccHand(val cards: Vector[Card]) {
    lazy val sum = (cards.foldLeft(0) { _ + _.bacc }) % 10
    def natural = sum == 8 || sum == 9
    def pat = natural || sum == 7 || sum == 6
    def :+(c: Card) = BaccHand(cards :+ c)
    override def toString = cards.mkString(" ")
  }

  private[this] def reprPair(pair: (Card, Card)) = "%s %s" format (
    pair._1, pair._2
  )
  private[this] def reprTriple(trip: (Card, Card, Card)) = "%s %s %s" format (
    trip._1, trip._2, trip._3
  )
}

