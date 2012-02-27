package com.yuvimasory.casinosim

import java.io.File

object Main {

  val usage =
"""
Usage: java -jar casinosim.jar game rounds

E.g., java -jar casinosim.jar craps 1000000

-- Misc --
Available games: craps, baccarat, war
""".trim


  def main(args: Array[String]) {
    if (args.length != 2) {
      Console.err println usage
      sys exit 1
    }
    else {
      if (args contains "-h") println(usage)
      else {
        val Array(gameName, roundsStr) = args
        val game = gameName match {
          case "craps"    => new Craps()
          case "coinflip" => new CoinFlip()
          case _       => {
            Console.err println { "unkown game: " + gameName }
            Console.err println()
            Console.err println usage
            sys exit 1
          }
        }
        val sim = new GameSim(game, roundsStr.toInt, None)
        sim runSim()
      }
    }
  }
}
