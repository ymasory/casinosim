package com.yuvimasory.cardgames

/* Main */
object Main {

  val Iters = 1000000

  def main(args: Array[String]) = baccarat()

  def baccarat() {
    val sim = new BaccaratSim
    sim start()
    sim ! Iters
  }

  def war() {
    val sim = new CasinoWarSim
    sim start()
    sim ! Iters
  }
}

