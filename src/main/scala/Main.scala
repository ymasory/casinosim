package com.yuvimasory.casinosim

import java.io.File

object Main {

  val usage =
"""
-- Mode 1, simulation --
Usage: java -jar casinosim.jar game rounds file [num decks]
E.g., java -jar casinosim.jar craps 10000 craps-output.sim
E.g., java -jar casinosim.jar war 20000 war-output.sim 6

-- Mode 2, analysis --
Usage: java -jar casinosim.jar analyze game file.sim
E.g., java -jar casinosim.jar analyze craps my-craps-file.sim

-- Misc --
6 decks is assumed if not specified for a card game
Available games: craps, baccarat, war
""".trim


  def main(args: Array[String]) {
    if (args contains "-h") println(usage)
    else {
      if (args.length < 3 || args.length > 4) {
        Console.err println usage
        sys exit 1
      }
      else {
        val vecArgs = Vector.empty ++ args
        if (args(0) == "analyze") analyze(vecArgs) else sim(vecArgs)
      }
    }
  }

  private[this] def analyze(args: Vector[String]) = {
    val Vector(_, gameName, fileName) = args
    gameName match {
      case "craps" => CrapsAnalyzer analyze new File(fileName)
    }
  }

  private[this] def sim(args: Vector[String]) = {
    val (game, rounds, file) = {
      try {
        val Vector(gameName, roundsStr, fileName, _*) = args
        val file = new File(fileName)
        val deckDesc = if (args.length < 4) FiniteDecks(6)
                       else DeckDescription fromInt args(3).toInt
        val rounds = roundsStr.toInt
        val game =
          gameName match {
            case "craps"    => new Craps()
            case "war"      => new War(deckDesc)
            case "baccarat" => new Baccarat(deckDesc)
          }
        (game, rounds, file)
      }
      catch {
        case e => {
          Console.err println "error parsing arguments"
          sys exit 1
        }
      }
    }
    val sim = new GameSim(game, rounds, file)
    sim runSim()
  }
}

