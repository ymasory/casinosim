package com.yuvimasory.cardgames

class Baccarat(numDecks: Int) extends Game(numDecks) {
  override def name = "Baccarat (Punto Banco)"
  override def play(): BaccaratState = {
    val shoe = createShoe
    val (Seq(p1, p2, b1, b2), postDrawShoe) = shoe draw 4
    val pt = sum(p1, p2)
    val bt = sum(b1, b2)
    if (natural(pt) && natural(bt)) chooseGreater(bt, pt)
    else if (natural(pt)) BaccaratState.player
    else if (natural(bt)) BaccaratState.banker
    else BaccaratState.empty
  }

  def chooseGreater(bt: Int, pt: Int) = {
    if (bt > pt) BaccaratState.banker
    else if (pt > bt) BaccaratState.player
    else BaccaratState.tie
  }
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
