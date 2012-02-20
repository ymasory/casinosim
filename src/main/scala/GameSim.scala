package com.yuvimasory.casinosim

import java.io.{ BufferedWriter, File, FileWriter }
import scala.actors.Actor

class GameSim(game: Game, rounds: Int, outFile: File) {

  private[this] val numTables = Runtime.getRuntime.availableProcessors
  val (roundsPerChunk, numChunks) = {
    val roundsPerTable = (rounds.toDouble/numTables.toDouble).ceil.toInt
    val roundsPerChunk = 1000 min (roundsPerTable.toDouble / 5).ceil.toInt
    val numChunks =
      (roundsPerTable.toDouble/roundsPerChunk.toDouble).ceil.toInt
    (roundsPerChunk, numChunks)
  }
  val trueNumRounds = roundsPerChunk * numChunks * numTables
  assert(
    trueNumRounds >= rounds,
    "true number of rounds %s less than requested number %s".format (
      trueNumRounds, rounds
    )
  )

  def runSim() {
    WriterActor start()
    for (i <- 0 until numTables) yield {
      Actor.actor {
        for (i <- 0 until numChunks) {
          val rounds =
            (0 until roundsPerChunk).foldLeft(Nil: List[game.GameRound]) {
              (acc, _) => (game play()) :: acc
            }
          WriterActor ! rounds
        }
        WriterActor ! TableDone
      }
    }
  }
  
  object WriterActor extends Actor {

    private[this] lazy val out = new BufferedWriter(new FileWriter(outFile))
    out write { "# playing %s on %s tables%n" format (game.name, numTables) }
    game match {
      case c: CardGame => out write { "# using %s%n" format c.deckDesc }
      case _ =>
    }
    out write { "# %s rounds requested%n" format rounds }
    out write { "# actually running %s rounds%n" format trueNumRounds }
    game.key match {
      case Some(key) => out write { "# format: %s%n" format key }
      case None =>
    }
    out write { "# %n" format() }

    private[this] var tablesDone = 0

    def act() = loop {
      react {
        case uRounds: List[_] => {
          if (uRounds.isEmpty) {
            out write { "# graceful exit%n" format() }
            out flush()
            out close()
            System.exit(0)
          }
          else {
            val rounds = uRounds.asInstanceOf[List[game.GameRound]]
            for (r <- rounds) {
              out write r.repr
              out newLine()
            }
          }
        }
        case TableDone => {
          tablesDone += 1
          if (tablesDone == numTables) { this ! Nil }
        }
      }
    }
  }

  case object TableDone
}
