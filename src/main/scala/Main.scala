package com.yuvimasory.cardgames

/* Main */
object Main {

  def main(args: Array[String]) {
    val sim = new CasinoWarSim
    sim.start()
    sim ! 100000000
  }
}

