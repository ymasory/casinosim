package com.yuvimasory.cardgames

/* Main */
object Main {

  val Iters = 100000

  def main(args: Array[String]) = baccarat()

  def baccarat() {
    val sim = new BaccaratSim
    sim start()
    sim ! Iters
  }

  def war() {
    val sim = WarSim next 1
    sim start()
    sim ! Iters
  }
}

