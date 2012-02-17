package com.yuvimasory.cardgames

class War(numDecks: Int) extends Game(numDecks) {
  override def name: String = "Casino War"
  override def play(): WarState = {
    val shoe = createShoe
    val (Seq(p, d), postDrawShoe) = shoe draw 2
    if (d > p) WarState.dealerRegWin
    else if (d < p) WarState.playerRegWin
    else {
      val postBurnShoe = postDrawShoe burn 3
      val (Seq(pw, dw), postTieShoe) = postBurnShoe draw 2
      if (dw > pw) WarState.dealerTieWin
      else         WarState.playerTieWin
    }
  }
}

case class WarState(
  val playerRegWins: Int,
  val playerTieWins: Int,
  val dealerRegWins: Int,
  val dealerTieWins: Int
) extends GameState {

  lazy val iterations =
    playerRegWins + playerTieWins + dealerRegWins + dealerTieWins

  lazy val regularWins = playerRegWins + dealerRegWins
  lazy val tieWins = playerTieWins + dealerTieWins
  lazy val playerRegularWinPercent: Double =
    (playerRegWins.toDouble / regularWins.toDouble) * 100
  lazy val dealerRegularWinPercent: Double =
    (dealerRegWins.toDouble / regularWins.toDouble) * 100
  lazy val dealerTieWinPercent: Double =
    (dealerTieWins.toDouble / tieWins.toDouble) * 100
  lazy val playerTieWinPercent: Double =
    (playerTieWins.toDouble / tieWins.toDouble) * 100

  lazy val houseEdgePercent: Double =
    (-1 * playerNet.toDouble / iterations.toDouble) * 100
  lazy val playerNet: Int =
    (playerTieWins + playerRegWins) - (dealerRegWins + (dealerTieWins * 2))

  override def summary(): String = """
War after %s rounds
Player regular wins: %s%%
Dealer regular wins: %s%%
Player tie wins: %s%%
Dealer tie wins: %s%%
House edge: %s%%
  """.trim.format(
    commaFmt format iterations,
    decFmt format playerRegularWinPercent,
    decFmt format dealerRegularWinPercent,
    decFmt format playerTieWinPercent,
    decFmt format dealerTieWinPercent,
    decFmt format houseEdgePercent
  )
  override def ++(g: GameState): WarState = {
    val that = g.asInstanceOf[WarState]
    WarState(
      playerRegWins + that.playerRegWins,
      playerTieWins + that.playerTieWins,
      playerRegWins + that.dealerRegWins,
      dealerTieWins + that.dealerTieWins
    )
  }
}

object WarState {
  def empty = WarState(0, 0, 0, 0)
  val playerTieWin = WarState(0, 1, 0, 0)
  val dealerTieWin = WarState(0, 0, 0, 1)
  val playerRegWin = WarState(1, 0, 0, 0)
  val dealerRegWin = WarState(0, 0, 1, 0)
}

class WarSim(game: Game) extends GameSim(game, WarState.empty)
object WarSim {
  def next(numDecks: Int): WarSim = new WarSim(new War(numDecks))
}
