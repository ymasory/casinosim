package com.yuvimasory.cardgames

/* Main */
object Main {

  val Iters = 1000000

  def main(args: Array[String]) = war()

  def war() {
    val sim = WarSim.mkSim(0)
    sim start()
    sim ! Iters
  }
}

