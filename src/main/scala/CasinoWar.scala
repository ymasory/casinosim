package com.yuvimasory.cardgames

import scala.actors.Actor

object CasinoWar {

  val shoe = InfiniteShoe()

  def play(hands: Int): GameState = {
    val firstState = GameState.empty
    val finalState =
      (1 to hands).foldLeft(firstState) { (acc: GameState, i: Int) =>
        val playerCard = shoe.deal
        val dealerCard = shoe.deal
        val playerProfit = (playerCard, dealerCard) match {
          case (p: PlayingCard, d: PlayingCard) =>
            if (d > p) -1 else if (d < p) 1 else {
              for (i <- 1 to 3) {
                val burn = shoe.deal
              }
              val pw = shoe.deal
              val dw = shoe.deal
              if (dw > pw) -2 else 1
            }
        }
        GameState(
          playerNet=(acc.playerNet + playerProfit),
          iterations=acc.iterations + 1
        )
      }
    finalState
  }

  case class GameState(
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
    def ++(that: GameState): GameState =
      GameState(playerNet + that.playerNet, iterations + that.iterations)
  }

  object GameState {
    def empty = GameState(0, 0)
  }
}


class CasinoWarSim() extends Actor {
  import CasinoWar.GameState

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
          println("all actors done")
          sys.exit(0)
        }
      }
    }
  }
}
