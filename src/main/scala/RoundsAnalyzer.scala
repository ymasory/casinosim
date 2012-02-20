package com.yuvimasory.casinosim

import java.io.{ BufferedReader, File, FileReader }

abstract class RoundsAnalyzer {

  val CommaFmt = new java.text.DecimalFormat("###,###,###,###")
  val DecFmt = new java.text.DecimalFormat("###.####")

  /* interface to implement */
  type MyStats <: AggregateStats
  type MyResult <: RoundResult

  val emptyStats: MyStats

  def parseLine(line: String): MyResult

  trait RoundResult
  trait AggregateStats {
    def :+(res: MyResult): MyStats
    def summary: String
  }

  final def genStats(file: File): MyStats = {
    val reader = new java.io.BufferedReader(new java.io.FileReader(file))
    this loop reader
  }


  /* private */
  private[this] def loop(
    in: BufferedReader,
    stats: MyStats = emptyStats): MyStats = {
      Option(in readLine()) match {
        case Some(line) => {
          val skip = line startsWith "#"
          if (skip) loop(in, stats)
          else loop(in, stats :+ parseLine(line))
        }
        case None => stats
      }
    }
}
