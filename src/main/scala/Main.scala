package com.yuvimasory.casinosim

import java.io.File

object Main {

  def main(args: Array[String]) = {
    if (args.length < 3 || args.length > 4) {
      Console.err println "Usage: java -jar casinosim.jar game rounds file [num decks]"
      Console.err println()
      Console.err println "E.g., java -jar casinosim.jar craps 10000 craps-output.sim"
      Console.err println "E.g., java -jar casinosim.jar war 20000 war-output.sim 6"
      Console.err println()
      Console.err println "6 decks is assumed if not specified for a card game"
      Console.err println "Available games: craps, baccarat, war"
    }
    val (game, rounds, file) = {
      try {
        val Array(gameName, roundsStr, fileName) = args
        val file = new File(fileName)
        val deckDesc = if (args.length < 4) FiniteDecks(6)
                       else DeckDescription fromInt args(3).toInt
        val rounds = roundsStr.toInt
        val game =
          gameName match {
            case "craps" => new Craps()
            case "war"   => new War(deckDesc)
          }
        (game, rounds, file)
      }
      catch {
        case e => {
          Console.err println "error parsing arguments"
          Console.err println e.getMessage
          sys exit 1
        }
      }
    }
    val sim = new GameSim(game, rounds, file)
    sim runSim()
  }
}

