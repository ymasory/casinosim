package com.yuvimasory.casinosim

import java.io.{ BufferedWriter, File, FileWriter }
import scala.actors.Actor

trait GameRound {
  def repr: String
}

sealed trait Game[A <: GameRound] {
  val desc: String
  val name: String
  def play(): A
}

abstract class CardGame[A <: GameRound](val numDecks: Int) extends Game[A] {
  override val desc = "%s with %s decks" format (name, numDecks)
  protected[this] def createShoe(): Shoe =
    if (numDecks <= 0) new InfiniteShoe()
    else FiniteShoe.next(numDecks)
}

trait DiceGame[A <: GameRound] extends Game[A] {
  override val desc = name
}

abstract class GameSim[A <: GameRound](game: Game[A], rounds: Int, outFile: File) {

  private[this] val numTables = Runtime.getRuntime.availableProcessors
  val (roundsPerChunk, numChunks) = {
    val roundsPerTable = (rounds.toDouble/numTables.toDouble).ceil.toInt
    val roundsPerChunk = 1000 min (roundsPerTable.toDouble / 5).ceil.toInt
    val numChunks = (roundsPerTable.toDouble/roundsPerChunk.toDouble).ceil.toInt
    (roundsPerChunk, numChunks)
  }
  val trueNumRounds = roundsPerChunk * numChunks * numTables
  assert(
    trueNumRounds >= rounds,
    "math error, true number of rounds %s is less than requested number %s".format (
      trueNumRounds, rounds
    )
  )

  def runSim() {
    WriterActor start()
    for (i <- 0 until numTables) yield {
      Actor.actor {
        for (i <- 0 until numChunks) {
          val rounds =
            (0 until roundsPerChunk).foldLeft(Nil: List[A]) { (acc, _) =>                
              (game play()) :: acc
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
    out write { "# %s rounds requested%n" format rounds }
    out write { "# actually running %s rounds%n" format trueNumRounds }
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
            val rounds = uRounds.asInstanceOf[List[A]]
            rounds foreach { r =>
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
