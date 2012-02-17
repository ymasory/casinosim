package com.yuvimasory.cardgames

class War(shoe: Shoe) extends Game(shoe) {
  def play(): WarState = {
    val p = shoe.deal
    val d = shoe.deal
    if (d > p) WarState.dealerRegWin
    else if (d < p) WarState.playerRegWin
    else {
      for (i <- 1 to 3) {
        val burn = shoe.deal
      }
      val pw = shoe.deal
      val dw = shoe.deal
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

  override def summary(shoe: Shoe): String = """
War after %s iterations using %s
Player Regular Wins: %s%%
Dealer Regular Wins: %s%%
Player Tie Wins: %s%%
Dealer Tie Wins: %s%%
House Edge: %s%%
  """.trim.format(
    commaFmt format iterations,
    shoe.summary,
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
  def mkSim(decks: Int): WarSim = {
    val shoe = if (decks <= 0) InfiniteShoe()
               else FiniteShoe(decks)
    new WarSim(new War(shoe))
  }
}
