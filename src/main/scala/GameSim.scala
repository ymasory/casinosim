package com.yuvimasory.casinosim

import java.io.{ BufferedWriter, File, FileWriter, OutputStreamWriter }

import scala.actors.Actor
import scala.annotation.tailrec

class GameSim(game: Game, rounds: Int, fileOpt: Option[File]) {

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
    println("Starting Simulation")
    WriterActor start()
    for (i <- 0 until numTables) yield {
      Actor.actor {
        for (i <- 0 until numChunks) {
          val rounds =
            (0 until roundsPerChunk).foldLeft(Nil: List[game.GameRound]) {
              (acc, _) => (game.player.play()) :: acc
            }
          WriterActor ! rounds
        }
        WriterActor ! TableDone
      }
    }
  }
  
  object WriterActor extends Actor {

    private[this] lazy val out: Option[BufferedWriter] =
      fileOpt.map { file: File => new BufferedWriter(new FileWriter(file)) }

    private[this] def record(s: String, fileOnly: Boolean = false) = {
      val str = "# %s%n" format s
      out match {
        case Some(o) => o write str
        case None => if (fileOnly == false) print(str)
      }
    }
    record("playing %s on %s tables" format (game.Name, numTables))
    game match {
      case c: CardGame => record("using %s" format c.deckDesc)
      case _ =>
    }
    record("%s rounds requested" format rounds.comma)
    record("actually going to run %s rounds" format trueNumRounds.comma)
    game.RoundKey match {
      case Some(key) => "format: %s" format key
      case None =>
    }

    private[this] var tablesDone = 0
    private[this] var counter = new game.OutcomeCounter
    private[this] var numRounds = 0

    def act() = loop {
      react {
        case uRounds: List[_] => {
          if (uRounds.isEmpty) {
            record("graceful exit")
            out match {
              case Some(o) => {
                o flush()
                o close()
              }
              case None =>
            }
            System.exit(0)
          }
          else {
            val rounds = uRounds.asInstanceOf[List[game.GameRound]]
            numRounds += rounds.length
            for (r <- rounds) {
              if (fileOpt.isDefined) record(r.serialize, true)
              counter = counter :+ r.asInstanceOf[game.MyGameRound]
            }
            if ((numRounds > 0 && numRounds % 1000000 == 0) || numRounds == trueNumRounds) {
              println()
              println("-" * 80)
              println(java.util.Calendar.getInstance.getTime)
              println("Summary After %s Rounds " format numRounds.comma)
              println("-" * 80)
              println(counter.summary)
              println()
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


