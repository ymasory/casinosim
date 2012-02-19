/*
package com.yuvimasory.casinosim

class Baccarat(numDecks: Int) extends CardGame(numDecks) {
  override def name = "Baccarat (Punto Banco)"
  override def play(): BaccaratState = {
    val shoe = createShoe
    val (Seq(p1, p2, b1, b2), postDrawShoe) = shoe draw 4
    val pt = sum(p1, p2)
    val bt = sum(b1, b2)
    if (natural(pt) && natural(bt)) chooseGreater(bt, pt)
    else if (natural(pt)) BaccaratState.player
    else if (natural(bt)) BaccaratState.banker
    else {
      if (pat(pt)) {
        if (pat(bt)) chooseGreater(bt, pt)
        else {
          val (Seq(b3), _) = postDrawShoe draw 1
          val bFinal = sum(bt, b3)
          chooseGreater(bFinal, pt)
        }
      }
      else {
        val (Seq(p3), postPlayer3Shoe) = postDrawShoe draw 1
        val pFinal = sum(pt, p3)
        val bFinal = {
          val p3Value = p3.rank.baccaratValue
          if (p3Value == 2 || p3Value == 3) {
            if (bt >= 0 && bt <= 4) drawFinal(postPlayer3Shoe, bt)
            else bt
          }
          if (p3Value == 4 || p3Value == 5) {
            if (bt >= 0 && bt <= 5) drawFinal(postPlayer3Shoe, bt)
            else bt
          }
          if (p3Value == 6 || p3Value == 7) {
            if (bt >= 0 && bt <= 6) drawFinal(postPlayer3Shoe, bt)
            else bt
          }
          if (p3Value == 8) {
            if (bt >= 0 && bt <= 2) drawFinal(postPlayer3Shoe, bt)
            else bt
          }
          else {
            if (bt >= 0 && bt <= 3) drawFinal(postPlayer3Shoe, bt)
            else bt
          }
        }
        chooseGreater(bFinal, pFinal)
      }
    }
  }

  def drawFinal(shoe: Shoe, tot: Int): Int = {
    val (Seq(lastCard), _) = shoe draw 1
    (tot + lastCard.rank.baccaratValue) % 10
  }
  def pat(i: Int) = (i == 6) || (i == 7)
  def chooseGreater(bt: Int, pt: Int) = {
    if (bt > pt) BaccaratState.banker
    else if (pt > bt) BaccaratState.player
    else BaccaratState.tie
  }
  def sum(i: Int, card: PlayingCard) = i + card.rank.baccaratValue
  def sum(cards: PlayingCard*) = {
    val ints = cards.map{ _.rank.baccaratValue }
    ints.sum % 10
  }
  def natural(i: Int) = i == 9 || i == 8
}

case class BaccaratState(
  val bankerWins: Int,
  val playerWins: Int,
  val ties: Int
) extends GameState {

  lazy val iterations = bankerWins + playerWins + ties
  lazy val playerWinPercent: Double =
    (playerWins.toDouble / iterations.toDouble) * 100
  lazy val bankerWinPercent: Double =
    (bankerWins.toDouble / iterations.toDouble) * 100
  lazy val tiePercent: Double =
    (ties.toDouble / iterations.toDouble) * 100

  lazy val tieHouseEdge =
    (-1 * tieNet.toDouble / iterations.toDouble) * 100
  lazy val playerHouseEdge =
    (-1 * playerNet.toDouble / iterations.toDouble) * 100
  lazy val bankerHouseEdge =
    (-1 * bankerNet.toDouble / iterations.toDouble) * 100

  lazy val tieNet = {
    val tieLosses = iterations - playerWins - bankerWins
    ties * 8 - tieLosses
  }
  lazy val playerNet = playerWins - bankerWins
  lazy val bankerNet = bankerWins * 0.95 - playerWins

  override def summary(): String = """
Baccarat after %s rounds
Player wins: %s%%
Player house edge: %s%%
Banker wins: %s%%
Banker house edge: %s%%
Tie: %s%%
Tie house edge: %s%%
  """.trim.format(
    commaFmt format iterations,
    decFmt format playerWinPercent,
    decFmt format playerHouseEdge,
    decFmt format bankerWinPercent,
    decFmt format bankerHouseEdge,
    decFmt format tiePercent,
    decFmt format tieHouseEdge
  )
  override def ++(g: GameState): BaccaratState = {
    val that = g.asInstanceOf[BaccaratState]
    BaccaratState(
      bankerWins = bankerWins + that.bankerWins,
      playerWins = playerWins + that.playerWins,
      ties = ties + that.ties
    )
  }
}

object BaccaratState {
  def empty = BaccaratState(0, 0, 0)
  def banker = BaccaratState(1, 0, 0)
  def player = BaccaratState(0, 1, 0)
  def tie = BaccaratState(0, 0, 1)
}

class BaccaratSim() extends GameSim(new Baccarat(6), BaccaratState.empty)
*/
