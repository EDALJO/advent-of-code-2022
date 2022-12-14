package com.github.edaljo

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.util.Try

object Day11 extends App {

  lazy val input = Source.fromFile("src/main/resources/day11.txt")
  val lines      = input.getLines().mkString("\n").split("\n\n").toList
//  val lines =
//    """Monkey 0:
//      |  Starting items: 79, 98
//      |  Operation: new = old * 19
//      |  Test: divisible by 23
//      |    If true: throw to monkey 2
//      |    If false: throw to monkey 3
//      |
//      |Monkey 1:
//      |  Starting items: 54, 65, 75, 74
//      |  Operation: new = old + 6
//      |  Test: divisible by 19
//      |    If true: throw to monkey 2
//      |    If false: throw to monkey 0
//      |
//      |Monkey 2:
//      |  Starting items: 79, 60, 97
//      |  Operation: new = old * old
//      |  Test: divisible by 13
//      |    If true: throw to monkey 1
//      |    If false: throw to monkey 3
//      |
//      |Monkey 3:
//      |  Starting items: 74
//      |  Operation: new = old + 3
//      |  Test: divisible by 17
//      |    If true: throw to monkey 0
//      |    If false: throw to monkey 1""".stripMargin.split("\n\n").toList

  case class Item(level: Long)
  enum Operation:
    case Addition, Multiplication, Division
  def parseOperation: String => Operation = {
    case "+" => Operation.Addition
    case "*" => Operation.Multiplication
    case "/" => Operation.Division
  }
  case class Action(op: Operation, n: Either[Int, String]) // Use Either to model the use of pure factor vs. old value
  case class Test(divisibleBy: Int, whenTrueTo: Int, whenFalseTo: Int)
  case class Monkey(items: List[Item], action: Action, test: Test, touches: Long = 0) {
    def handleItems(fun: Long => Long): SortedMap[Int, List[Item]] =
      SortedMap.from {
        items.map { item =>
          val worryLevel = action match
            case Action(Operation.Addition, Left(i))        => item.level + i
            case Action(Operation.Addition, Right(_))       => item.level + item.level // To add old value
            case Action(Operation.Multiplication, Left(i))  => item.level * i
            case Action(Operation.Multiplication, Right(_)) => item.level * item.level // To multiply with old value
            case _ => throw new Exception("No other operations should be supported")

          fun(worryLevel) match
            case n if n % test.divisibleBy == 0 => test.whenTrueTo -> item.copy(level = n)
            case n => test.whenFalseTo -> item.copy(level = n)
        }.groupMap(_._1)(_._2)
      }

    def receiveItems(items: List[Item]): Monkey   = this.copy(items = this.items ++ items)
    def clearItemsAndIncreaseTouchCount(): Monkey = this.copy(items = List.empty, touches = this.touches + items.size)
  }

  def parse(lines: List[String]): SortedMap[Int, Monkey] = lines.foldLeft(SortedMap.empty[Int, Monkey]) {
    case (monkeys, l) =>
      l.split("\n").toList match
        case ::(head, next) =>
          val monkeyNo = head.strip.split(":").head.last.toString.toInt
          val startingItems =
            next.head.split(":").last.strip.split(" ").map(_.replace(",", "").toInt).map(Item(_)).toList
          val action = next(1).split("= old").last.strip.split(" ").toList match
            case List(op, n) =>
              Action(
                parseOperation(op),
                Try(Left(n.toInt))
                  .getOrElse(if n == "old" then Right(n) else throw new Exception("Couldn't parse operation"))
              )
          val divisibleBy = next(2).split("divisible by").last.strip.toInt
          val whenTrue    = next(3).split("to monkey").last.strip.toInt
          val whenFalse   = next(4).split("to monkey").last.strip.toInt
          val test        = Test(divisibleBy, whenTrue, whenFalse)
          monkeys.updated(monkeyNo, Monkey(startingItems, action, test))
        case Nil => monkeys
  }

  def observeMonkeys(numberOfRounds: Int, startConfig: SortedMap[Int, Monkey])(
    fun: Long => Long
  ): SortedMap[Int, Monkey] =
    (1 to numberOfRounds).foldLeft(startConfig) { case (currentRoundConfig, _) =>
      currentRoundConfig.foldLeft(currentRoundConfig) { case (currentTurnConfig, (i, _)) =>
        currentTurnConfig(i)
          .handleItems(fun) // The monkey who's turn it is currently
          .foldLeft(currentTurnConfig) { case (configToUpdate, (toMonkeyNo, updatedItemList)) =>
            // Update monkeys receiving items
            configToUpdate.updated(toMonkeyNo, configToUpdate(toMonkeyNo).receiveItems(updatedItemList))
          }
          // Update current monkey's items and total touch count
          .updated(i, currentTurnConfig(i).clearItemsAndIncreaseTouchCount())
      }
    }

  def computeMonkeyBusiness(itemTouchCount: SortedMap[Int, Monkey]): Long =
    itemTouchCount.values.map(_.touches).toList.sorted.takeRight(2).product

  val startConfig = parse(lines)
  lazy val gcd    = startConfig.values.map(_.test.divisibleBy).product.toLong

  // Part 1
  val finalState = observeMonkeys(20, startConfig)(_ % gcd / 3)
  val result1    = computeMonkeyBusiness(finalState)

  println(s"Result pt. 1: $result1")

  // Part 2
  val finalState2 = observeMonkeys(10000, startConfig)(_ % gcd)
  val result2     = computeMonkeyBusiness(finalState2)
  println(s"Result pt. 2: $result2")

  input.close()

}
