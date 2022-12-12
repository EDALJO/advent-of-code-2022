package com.github.edaljo

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day10 extends App {

  lazy val input = Source.fromFile("src/main/resources/day10.txt")
  val lines      = input.getLines().toList

  // Part 1
  val register = lines
    .foldLeft(Vector(1)) { case (register, s) =>
      s.split(" ").toList match
        case List("noop") => register :+ register.last
        // Duplicate value as it takes 2 cycles to complete, hence index becomes cycle number
        case List("addx", i) if Try(i.toInt).isSuccess =>
          val last = register.last
          register ++ Vector(last, last + i.toInt)
    }

  val result1 = (20 to register.size by 40).map(i => i * register(i - 1)).sum

  println(s"Result pt. 1: $result1")

  // Part 2
  val result2 = register.tail.zipWithIndex.map { case (x, i) =>
    if math.abs((i + 1) - (i / 40) * 40 - x) <= 1 then "#" else "."
  }
  println(s"Result pt. 2:")
  result2.grouped(40).map(_.mkString(" ")).foreach(println)

  input.close()

}
