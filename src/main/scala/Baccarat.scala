/*
package com.yuvimasory.cardgames

import scala.actors.Actor

object Baccarat {

  val shoe = InfiniteShoe()

  def play(hands: Int): GameState = {
    val firstState = GameState.empty
    val finalState =
      (1 to hands).foldLeft(firstState) { (acc: GameState, i: Int) =>
        
      }
    finalState
  }

  case class GameState(
    val playerWins: Int,
    val bankerWins: Int,
    val ties: Int
  ) {
    private[this] val fmt = new java.text.DecimalFormat("###.###")

    val iterations = playerWins + bankerWins + ties
    val bankerNet: Int = {
      val bankerLosses = iterations - bankerWins - ties
      iterations - bankerLosses + (bankerWins * 0.95).floor.toInt
    }
    val tieNet = {
      val tieLosses = iterations - bankerWins - playerWins
      iterations - tieLosses + ties * 8
    }
    val playerNet = {
      val playerLosses = iterations - bankerWins - ties
      iterations - playerLosses + playerWins
    }

    def summary: String =
"""
Baccarat after %s iterations using a/an %s
Player won: %s%%
Player house edge: %s%%
Banker won: %s%%
Banker house edge: %s%%
Ties: %s%%
Tie house edge: %s%%
""".trim.format(
     iterations,
     shoe,
     winsPercentStr(playerWins),
     edgeStr(playerNet),
     winsPercentStr(bankerWins),
     edgeStr(bankerNet),
     winsPercentStr(ties),
     edgeStr(tieNet)
   )
    private[this] def winsPercentStr(wins: Int) =
      fmt format { (wins.toDouble / iterations.toDouble) * 100 }

    private[this] def edgeStr(net: Int) =
      fmt format (1 - (iterations.toDouble / net.toDouble))

    def ++(that: GameState): GameState = GameState(
      playerWins + that.playerWins,
      bankerWins + that.bankerWins,
      ties + that.ties
    )
  }

  object GameState {
    def empty = GameState(0, 0, 0)
  }
}

class BaccaratSim() extends Actor {
  import Baccarat.GameState

  private[this] var runningState: GameState = GameState.empty
  private[this] val numTables = Runtime.getRuntime.availableProcessors
  private[this] var tablesDone = 0

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
              val state = Baccarat play chunk
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
*/
