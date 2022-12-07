package com.github.edaljo

import scala.io.Source

object Day6 extends App {

  lazy val input = Source.fromFile("src/main/resources/day6.txt")

  lazy val chars = input.getLines().toList.head.toCharArray.toList

  def findDistinctSubsequence(chars: List[Char], windowSide: Int): Int =
    chars
      .sliding(windowSide)
      .zipWithIndex
      .collectFirst { case (l, i) if l.toSet.size == windowSide => i + windowSide }
      .get

  // Part 1
  val result1 = findDistinctSubsequence(chars, 4)
  println(s"Result pt. 1: $result1")

  // Part 2
  val result2 = findDistinctSubsequence(chars, 14)
  println(s"Result pt. 2: $result2")

  input.close()

}
