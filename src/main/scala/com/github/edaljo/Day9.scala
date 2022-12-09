package com.github.edaljo

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {

  lazy val input = Source.fromFile("src/main/resources/day9.txt")
  val lines      = input.getLines().toList

  val moves = lines
    .map(_.split(" ").toList match {
      case List(s, i) => parseHeadMove(s, i.toInt);
      case t          => throw new Exception(t.toString)
    })

  def parseHeadMove(direction: String, steps: Int): Move = direction match {
    case "R" => Move(steps, Direction.Right)
    case "D" => Move(steps, Direction.Down)
    case "L" => Move(steps, Direction.Left)
    case "U" => Move(steps, Direction.Up)
  }

  enum Direction:
    case Right, Down, Left, Up
  case class Move(steps: Int, direction: Direction)

  case class Coordinates(x: Int, y: Int)
  sealed trait Position { val coordinates: Coordinates }
  sealed trait Head extends Position {
    self: Position =>
    def updatePosition(move: Move): Head = move match {
      case Move(_, Direction.Right) => HeadPosition(Coordinates(this.coordinates.x + 1, this.coordinates.y))
      case Move(_, Direction.Down)  => HeadPosition(Coordinates(this.coordinates.x, this.coordinates.y - 1))
      case Move(_, Direction.Left)  => HeadPosition(Coordinates(this.coordinates.x - 1, this.coordinates.y))
      case Move(_, Direction.Up)    => HeadPosition(Coordinates(this.coordinates.x, this.coordinates.y + 1))
    }
  }
  sealed trait Tail extends Position {
    def updatePosition(positionInFront: Position): Tail =
      Coordinates(
        positionInFront.coordinates.x - this.coordinates.x,
        positionInFront.coordinates.y - this.coordinates.y
      ) match
        // In every dimension, if the absolute diff is at least 1 coordinate, make a unit move in given direction
        case Coordinates(dx, dy) if math.abs(dy) > 1 || math.abs(dx) > 1 =>
          TailPosition(Coordinates(this.coordinates.x + dx.sign * 1, this.coordinates.y + dy.sign * 1))
        case _ => this
  }

  case class HeadPosition(coordinates: Coordinates) extends Head
  case class TailPosition(coordinates: Coordinates) extends Tail

  @tailrec
  def applyMove(
    move: Move,
    headKnot: Head,
    tailKnots: List[Tail],
    tailVisitedCoordinates: Set[Coordinates]
  ): (Head, List[Tail], Set[Coordinates]) =
    if move.steps == 0 then (headKnot, tailKnots, tailVisitedCoordinates)
    else
      val updatedMove          = move.copy(steps = move.steps - 1)
      val updatedHeadPosition  = headKnot.updatePosition(updatedMove)
      val updatedFirstTailKnot = tailKnots.head.updatePosition(updatedHeadPosition)
      val updatedTailPosition = tailKnots.tail.foldLeft(List(updatedFirstTailKnot)) { case (updatedPositions, knot) =>
        updatedPositions :+ knot.updatePosition(updatedPositions.last)
      }
      applyMove(
        updatedMove,
        updatedHeadPosition,
        updatedTailPosition,
        tailVisitedCoordinates + updatedTailPosition.last.coordinates
      )

  @tailrec
  def traverse(
    moves: List[Move],
    headKnot: Head,
    tailKnots: List[Tail],
    tailVisistedCoordinates: Set[Coordinates]
  ): Set[Coordinates] = moves match
    case Nil => tailVisistedCoordinates
    case ::(head, next) =>
      val (updatedHead, updatedTail, updatedVisitedCoordinates) =
        applyMove(head, headKnot, tailKnots, tailVisistedCoordinates)
      traverse(next, updatedHead, updatedTail, updatedVisitedCoordinates)

  val origin = Coordinates(0, 0)

  // Part 1
  val result1 = traverse(moves, HeadPosition(origin), List(TailPosition(origin)), Set(origin))
  println(s"Result pt. 1: ${result1.size}")

  // Part 2
  val result2 = traverse(moves, HeadPosition(origin), (1 to 9).map(_ => TailPosition(origin)).toList, Set(origin))
  println(s"Result pt. 2: ${result2.size}")

  input.close()

}
