package com.github.edaljo

import com.github.edaljo.Day2.oppositeAction

import scala.io.Source

object Day2 extends App {

  trait Points { val points: Int }

  sealed trait Action extends Points {
    val drawsWith: Action = this
    val winsOver: Action
    val losesAgainst: Action
  }
  case object Rock extends Action {
    override val points: Int          = 1
    override val winsOver: Action     = Scissors
    override val losesAgainst: Action = Paper
  }
  case object Paper extends Action {
    override val points: Int          = 2
    override val winsOver: Action     = Rock
    override val losesAgainst: Action = Scissors
  }
  case object Scissors extends Action {
    override val points: Int          = 3
    override val winsOver: Action     = Paper
    override val losesAgainst: Action = Rock
  }

  sealed trait Outcome extends Points {
    val action: Action
    val points: Int
    lazy val totalPoints = points + action.points
  }
  case class Loss(action: Action) extends Outcome { override val points: Int = 0 }
  case class Draw(action: Action) extends Outcome { override val points: Int = 3 }
  case class Win(action: Action)  extends Outcome { override val points: Int = 6 }

  val actionFromString: String => Action = {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
  }

  val outcomeMap: (Action, Action) => (Outcome, Outcome) = {
    case (a1, a2) if a1 == a2                 => (Draw(a1), Draw(a2))
    case (rock @ Rock, scissors @ Scissors)   => (Win(rock), Loss(scissors))
    case (scissors @ Scissors, rock @ Rock)   => (Loss(scissors), Win(rock))
    case (scissors @ Scissors, paper @ Paper) => (Win(scissors), Loss(paper))
    case (paper @ Paper, scissors @ Scissors) => (Loss(paper), Win(scissors))
    case (paper @ Paper, rock @ Rock)         => (Win(paper), Loss(rock))
    case (rock @ Rock, paper @ Paper)         => (Loss(rock), Win(paper))
  }

  lazy val input = Source.fromFile("src/main/resources/day2.txt")

  val lines = input
    .getLines()
    .toList

  lazy val resultsFromActions: (String => (Action, String => Action)) => (Int, Int) = fComposed =>
    lines.map {
      _.split(" ") match {
        case Array(s1, s2) =>
          val (opponentAction, action) = fComposed(s1)
          (opponentAction, action(s2))
        case l => throw new IllegalArgumentException(s"Should not encounter $l")
      }
    }
      .foldLeft((0, 0)) { case ((s1, s2), (a1, a2)) =>
        val (o1, o2) = outcomeMap(a1, a2)
        (s1 + o1.totalPoints, s2 + o2.totalPoints)
      }

  // Part 1
  lazy val result1 = resultsFromActions(s => (actionFromString(s), actionFromString))

  println(s"Result pt. 1: $result1")

  // Part 2
  val oppositeAction: String => Action => Action = {
    case "X" => _.winsOver
    case "Y" => _.drawsWith
    case "Z" => _.losesAgainst
  }

  val result2 = resultsFromActions { s =>
    val opponentAction = actionFromString(s)
    (opponentAction, oppositeAction(_)(opponentAction))
  }

  println(s"Result pt. 2: $result2")

  input.close()

}
