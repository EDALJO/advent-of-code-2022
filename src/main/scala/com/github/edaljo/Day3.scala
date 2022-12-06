package com.github.edaljo

import scala.io.Source

object Day3 extends App {

  lazy val input = Source.fromFile("src/main/resources/day3.txt")

  val lines = input
    .getLines()
    .toList

  def priorities: PartialFunction[Char, Int] = {
    case s if s.isLower => s.toInt - 96
    case s if s.isUpper => s.toInt - 38
  }

  // Part 1
  val result1 = lines.map { s =>
    s.grouped(s.length / 2).toList match
      case List(s1, s2) if s1.length == s2.length =>
        val intersection = s1.toSet.intersect(s2.toSet)
        intersection.collect(priorities).sum
      case _ => throw new Exception("String wasn't split in 2 parts")
  }.sum

  println(s"Result pt. 1: $result1")

  // Part 2
  val result2 = lines
    .grouped(3)
    .map {
      case List(s1, s2, s3) => s1.toSet.intersect(s2.toSet).intersect(s3.toSet).collect(priorities).sum
      case _                => throw new Exception("Group didn't consist of 3 item lists")
    }
    .sum

  println(s"Result pt. 2: $result2")

  input.close()

}
