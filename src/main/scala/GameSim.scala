package com.yuvimasory.casinosim

import scala.actors.Actor

trait GameState {
  def ++(that: GameState): GameState
  def summary(): String
}
sealed trait Game {
  def name: String
  def play(): GameState
}

abstract class CardGame(val numDecks: Int) extends Game {
  def createShoe: Shoe = {
    if (numDecks <= 0) new InfiniteShoe()
    else FiniteShoe.next(numDecks)
  }
}
trait DiceGame extends Game

abstract class GameSim(game: Game, emptyState: GameState) extends Actor {

  protected[this] var runningState = emptyState
  private[this] val numTables = Runtime.getRuntime.availableProcessors
  private[this] var tablesDone = 0

  println()
  println("playing %s on %s tables" format (game.name, numTables))
  game match {
    case cardGame: CardGame => println("using " + cardGame.createShoe.summary)
    case _ =>
  }

  def act() = loop {
    react {
      case iters: Int => {
        println((commaFmt format iters) + " iters requested")
        val share = (iters.toDouble/numTables.toDouble).ceil.toInt
        for (i <- 0 until numTables) yield {
          Actor.actor {
            val chunk = 250000 min (iters/5).toInt
            val numChunks = (share.toDouble/chunk.toDouble).ceil.toInt
            for (i <- 0 until numChunks) {
              val finalState =
                (1 to chunk).foldLeft(emptyState) { (acc: GameState, i: Int) =>
                  val resState: GameState = game play()
                  resState ++ acc
                }
              this ! finalState
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
          println()
          println("ALL ACTORS DONE")
          sys.exit(0)
        }
      }
    }
  }
}
