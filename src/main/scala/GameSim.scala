package com.yuvimasory.cardgames

import scala.actors.Actor

trait GameState {
  def ++(that: GameState): GameState
  def summary(shoe: Shoe): String
}
abstract class Game(val shoe: Shoe) {
  def play(): GameState
}

abstract class GameSim(game: Game, emptyState: GameState) extends Actor {

  protected[this] var runningState = emptyState

  private[this] val shoe = game.shoe
  private[this] val numTables = 4
  private[this] var tablesDone = 0
  private[this] val chunk = 250000

  println()
  println("playing on %s tables" format numTables)

  def act() = loop {
    react {
      case iters: Int => {
        println((commaFmt format iters) + " iters requested")
        val share = (iters.toDouble/numTables.toDouble).ceil.toInt
        for (i <- 0 until numTables) yield {
          Actor.actor {
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
        println(runningState summary shoe)
        if (tablesDone == numTables) {
          println()
          println("ALL ACTORS DONE")
          sys.exit(0)
        }
      }
    }
  }
}