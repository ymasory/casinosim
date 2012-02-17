package com.yuvimasory.cardgames

import scala.actors.Actor

object CasinoWar {

  def play(hands: Int): GameState = {
    val firstState = GameState.empty
    val finalState =
      (1 to hands).foldLeft(firstState) { (acc: GameState, i: Int) =>
        val (playerCard, _) = acc.shoe.deal
        val (dealerCard, peaceShoe: InfiniteShoe) = acc.shoe.deal
        val (playerProfit, nShoe) = (playerCard, dealerCard) match {
          case (p: PlayingCard, d: PlayingCard) =>
            if (d > p)      (-1, peaceShoe)
            else if (d < p) (1, peaceShoe)
            else {
              for (i <- 1 to 3) {
                val(burn, _) = acc.shoe.deal
              }
              val (pw, _) = acc.shoe.deal
              val (dw, warShoe) = acc.shoe.deal
              if (dw > pw) (-2, warShoe) else (1, warShoe)
            }
        }
        GameState(
          shoe=nShoe,
          playerNet=(acc.playerNet + playerProfit),
          iterations=acc.iterations + 1
        )
      }
    finalState
  }
}

case class GameState(
  val shoe: InfiniteShoe,
  val playerNet: Double,
  val iterations: Int
) {
  private[this] val fmt = new java.text.DecimalFormat("###.###")
  private[this] val bigFmt = new java.text.DecimalFormat("###,###,###,###")
  def houseEdge: Double = ((playerNet/iterations) * 100) * -1
  def summary: String =
"""
CasinoWar after %s iterations using a/an %s
Player Net: %s
House Edge: %s%%
""".trim.format(
     bigFmt format iterations,
     shoe.summary,
     playerNet,
     fmt format houseEdge
   )
  def ++(that: GameState): GameState = {
    if (shoe != that.shoe)
      sys.error("cannot combine shoes %s and %s" format (shoe, that.shoe))
    GameState(shoe, playerNet + that.playerNet, iterations + that.iterations)
  }
}

object GameState {
  def empty = GameState(new InfiniteShoe(), 0, 0)
}

class CasinoWarSim() extends Actor {

  private[this] var runningState: GameState = GameState.empty
  private[this] val numTables = Runtime.getRuntime.availableProcessors
  private[this] var tablesDone = 0

  println("playing on %s tables" format numTables)

  def act() = loop {
    react {
      case iters: Int => {
        println("%s iters requested" format iters)
        val share = (iters.toDouble/numTables.toDouble).ceil.toInt
        for (i <- 0 until numTables) yield {
          Actor.actor {
            val chunk = 250000
            val numChunks = (share.toDouble/chunk.toDouble).ceil.toInt
            for (i <- 0 until numChunks) {
              val state = CasinoWar play chunk
              this ! state
            }
            tablesDone += 1
          }
        }
      }
      case state: GameState => {
        runningState = runningState ++ state
        println()
        println(runningState.summary)
        if (tablesDone == numTables) {
          println("all threads done")
          sys.exit(0)
        }
      }
      case e => Console.err.println("unhandled " + e)
    }
  }
}
