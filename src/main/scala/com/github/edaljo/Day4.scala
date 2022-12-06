package com.github.edaljo

import scala.io.Source

object Day4 extends App {

  lazy val input = Source.fromFile("src/main/resources/day4.txt")

  lazy val parsedIntervals = input
    .getLines()
    .toList
    .map(_.split(",").map(parseInterval).toList match
      case List(i1, i2) => (i1, i2)
      case _            => throw new Exception("Row didn't consist of two intervals.")
    )

  case class Interval(lower: Int, upper: Int) {
    def fullyOverlaps(other: Interval): Boolean = lower <= other.lower && upper >= other.upper
    def overlapsPartially(other: Interval): Int = if upper >= other.lower && lower <= other.upper then 1 else 0
  }

  val parseInterval: String => Interval = _.split("-").map(_.toInt) match
    case Array(i1, i2) => Interval(i1, i2)
    case _ =>
      throw new Exception("Interval didn't consist of an upper and a lower bound")

  // Part 1
  val result1 = parsedIntervals.map { case (i1, i2) =>
    if i1.fullyOverlaps(i2) || i2.fullyOverlaps(i1) then 1 else 0
  }.sum

  println(s"Result pt. 1: $result1")

  // Part 2
  val result2 = parsedIntervals.map { case (i1, i2) => i1.overlapsPartially(i2) }.sum

  println(s"Result pt. 2: $result2")

  input.close()

}
