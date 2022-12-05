package com.github.edaljo

import scala.io.Source
import scala.util.Try

object Day1 extends App {

  lazy val input = Source.fromFile("src/main/resources/day1.txt")
  lazy val grouped = input
    .getLines()
    .toList
    .mkString(",")
    .split(",,")
    .map(_.split(",").map(_.toInt).sum)

  // Part 1
  println(grouped.max)

  // Part 2
  println(grouped.zipWithIndex.sortBy(-_._1).take(3).map(_._1).sum)

  input.close()

}
