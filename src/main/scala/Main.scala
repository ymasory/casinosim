package com.yuvimasory.casinosim

/* Main */
object Main {

  def main(args: Array[String]) = {
    if (args.length != 3) {
      Console.err println "Usage: java -jar casinosim.jar [game] [rounds] [file]"
      Console.err println()
      Console.err println "E.g., java -jar casinosim.jar craps 10000 output.sim"
      Console.err println "Available games: craps, baccarat, casino war"
    }
    val Array(gameName, roundsStr, fileName) = args
    val rounds = roundsStr.toInt
    val game = new Craps()
    val sim = new GameSim(game, rounds, new java.io.File(fileName)) {}
    sim runSim()
  }
}

