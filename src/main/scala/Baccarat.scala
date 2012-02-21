/*
package com.yuvimasory.casinosim

class Baccarat(desc: DeckDescription) extends CardGame(desc) {

  override val name = "Baccarat (Punto Banco)"
  override val key = Some(
    "player1 player2 [player3] - banker1 banker2 [banker3]"
  )

  override def play(): BaccaratRound = {
    val shoe = createShoe
    val (List(p1, p2, b1, b2), postDrawShoe) = shoe draw 4
    val ph1 = BaccaratHand(Vector(p1, p2))
    val bh1 = BaccaratHand(Vector(b1, b2))
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
    BaccaratRound(phFinal, bhFinal)
  }

  case class BaccaratHand(val cards: Vector[Card]) {
    lazy val sum = (cards.foldLeft(0) { _ + _.bacc }) % 10
    def natural = sum == 8 || sum == 9
    def pat = natural || sum == 7 || sum == 6
    def :+(c: Card) = BaccaratHand(cards :+ c)
    override def toString = cards.mkString(" ")
  }

  case class BaccaratRound(pHand: BaccaratHand, bHand: BaccaratHand)
    extends GameRound {
    override def repr = "%s - %s" format (pHand, bHand)
  }

  private[this] def reprPair(pair: (Card, Card)) = "%s %s" format (
    pair._1, pair._2
  )
  private[this] def reprTriple(trip: (Card, Card, Card)) = "%s %s %s" format (
    trip._1, trip._2, trip._3
  )
}
*/
