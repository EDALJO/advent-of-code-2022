package com.github.edaljo

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day5 extends App {

  lazy val input = Source.fromFile("src/main/resources/day5.txt")

  lazy val lines = input
    .getLines()
    .toList

  val supplyStack = lines
    .takeWhile(_.nonEmpty)
    .map(_.grouped(4).map("[A-Z1-9]".r.findFirstIn).toList)
    .transpose
    .map(_.flatten match
      case ::(head, tail) => (tail.last.toInt, head :: tail.dropRight(1))
      case _              => throw new Exception("Failed parsing the initial stack rearrangement")
    )
    .toMap

  val instructions = lines
    .dropWhile(_.nonEmpty)
    .filterNot(_.isEmpty)
    .map("([0-9]+)".r.findAllIn)
    .map(_.toList.map(_.toInt) match
      case List(i1, i2, i3) => Instruction(i1, i2, i3)
      case _                => throw new Exception("Failed parsing the stack rearrangement instructions")
    )

  case class Instruction(
    numberOfCrates: Int,
    fromStack: Int,
    toStack: Int
  ) {
    def updateStack(stacks: Map[Int, List[String]], reverse: Boolean): Map[Int, List[String]] =
      val currentStack = stacks(fromStack)
      val movedStack =
        if reverse then currentStack.take(numberOfCrates).reverse
        else currentStack.take(numberOfCrates)
      stacks
        .updated(toStack, movedStack ++ stacks(toStack))
        .updated(fromStack, currentStack.drop(numberOfCrates))
  }

  @tailrec
  def rearrangeStack(
    stacks: Map[Int, List[String]],
    instructions: List[Instruction],
    reverse: Boolean
  ): Map[Int, List[String]] = instructions match
    case Nil                   => stacks
    case ::(instruction, next) => rearrangeStack(instruction.updateStack(stacks, reverse), next, reverse)

  def generateAnswer(stacks: Map[Int, List[String]], instructions: List[Instruction], reverse: Boolean): String =
    val rearrangedStack = rearrangeStack(stacks, instructions, reverse)
    (1 to stacks.size)
      .map(rearrangedStack(_).headOption.getOrElse(""))
      .mkString("")

  // Part 1
  val result1 = generateAnswer(supplyStack, instructions, reverse = true)
  println(s"Result pt. 1: $result1")

  // Part 2
  val result2 = generateAnswer(supplyStack, instructions, reverse = false)
  println(s"Result pt. 2: $result2")

  input.close()

}
