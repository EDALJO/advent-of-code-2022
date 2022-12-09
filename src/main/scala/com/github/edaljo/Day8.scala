package com.github.edaljo

import java.text.ParseException
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day8 extends App {

  lazy val input = Source.fromFile("src/main/resources/day8.txt")
  val lines          = input.getLines().toList
//  val lines =
//    """30373
//      |25512
//      |65332
//      |33549
//      |35390""".stripMargin.split("\n").toList
  val grid           = lines.map(_.map(_.toString.toInt).toVector).toVector
  val gridTransposed = grid.transpose
  val width          = grid.head.size
  val height         = grid.size

  // Part 1
  var visibleInterior = Map.empty[(Int, Int), Int]

  for (i <- 1 to width - 2) {
    for (j <- 1 to height - 2) {
      val treeHeight   = grid(i)(j)
      val visibleLeft  = grid(i).take(j).forall(_ < treeHeight)
      val visibleRight = grid(i).takeRight(width - 1 - j).forall(_ < treeHeight)
      val visibleUp    = gridTransposed(j).take(i).forall(_ < treeHeight)
      val visibleDown  = gridTransposed(j).takeRight(height - 1 - i).forall(_ < treeHeight)
      visibleInterior =
        if visibleLeft || visibleRight || visibleUp || visibleDown
        then visibleInterior.updated((i, j), treeHeight)
        else visibleInterior
    }
  }

  val result1 = visibleInterior.size + 2 * (height + width - 2)
  println(s"Result pt. 1: $result1")

  // Part 2
  var scenicScore = Map.empty[(Int, Int), Int]

  for (i <- 0 until width - 1) {
    for (j <- 0 until height - 1) {
      val treeHeight = grid(i)(j)
      val scoreLeft  = if j > 0 then math.min(grid(i).take(j).reverse.takeWhile(_ < treeHeight).size + 1, j) else 0
      val scoreRight =
        if j < width - 1 then math.min(grid(i).takeRight(width - 1 - j).takeWhile(_ < treeHeight).size + 1, width - 1 - j)
        else 0
      val scoreUp = if i > 0 then math.min(gridTransposed(j).take(i).reverse.takeWhile(_ < treeHeight).size + 1, i) else 0
      val scoreDown =
        if i < height - 1 then
          math.min(gridTransposed(j).takeRight(height - 1 - i).takeWhile(_ < treeHeight).size + 1, height - 1 - i)
        else 0
      val treeScenicScore = scoreLeft * scoreRight * scoreUp * scoreDown
      scenicScore = scenicScore.updated((i, j), treeScenicScore)
    }
  }

  val result2 = scenicScore.maxBy(_._2)._2

  println(s"Result pt. 2: $result2")

  input.close()

}
