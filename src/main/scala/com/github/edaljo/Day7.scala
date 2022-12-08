package com.github.edaljo

import java.text.ParseException
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day7 extends App {

  lazy val input = Source.fromFile("src/main/resources/day7.txt")
  val lines      = input.getLines().toList

  @tailrec
  def updateMap(
    sizeMap: Map[String, Int],
    parentDirMap: Map[String, String],
    currentDirectory: String,
    sizeToAdd: Int
  ): Map[String, Int] = parentDirMap.get(currentDirectory) match
    case None => sizeMap
    case Some(parentDir) =>
      val newSize = sizeMap.getOrElse(parentDir, 0) + sizeToAdd
      updateMap(sizeMap.updated(parentDir, newSize), parentDirMap, parentDir, sizeToAdd)

  @tailrec
  def filesystemToMaps(
    cmds: List[String],
    parentDirMap: Map[String, String], // Current -> Parent
    fsMap: Map[String, Int],
    current: String,
    parent: Option[String]
  ): (Map[String, String], Map[String, Int]) = cmds match
    case Nil => (parentDirMap, fsMap)
    case ::(head, next) =>
      head.split("""\s""").toList match
        case List("$", "cd", "..") =>
          val newParent = parent.flatMap(parentDirMap.get)
          filesystemToMaps(next, parentDirMap, fsMap, parent.getOrElse(root), newParent)
        case List("$", "cd", dir) =>
          filesystemToMaps(next,
                           parentDirMap,
                           fsMap,
                           current.stripSuffix("/") + "/" + dir.stripSuffix("/"),
                           if dir != root then Some(current) else None
          )
        case List("$", "ls") =>
          val dirContent = next.takeWhile(s => !s.contains("$"))
          val (updatedParentDirMap, updatedFsMap) =
            dirContent.foldLeft((parentDirMap, fsMap)) { case ((pm, fsm), s) =>
              s.split("""\s""").toList match
                case List(i, _) if Try(i.toInt).isSuccess =>
                  val currentSize = fsm.getOrElse(current, 0)
                  (pm, fsm.updated(current, currentSize + i.toInt))
                case List("dir", dirName) =>
                  (pm.updated(current.stripSuffix("/") + "/" + dirName.stripSuffix("/"), current), fsm)
            }
          // Assumes every dir is visited only once
          val updatedFsMap2 = updateMap(updatedFsMap, updatedParentDirMap, current, updatedFsMap.getOrElse(current, 0))
          filesystemToMaps(next.dropWhile(s => !s.contains("$")), updatedParentDirMap, updatedFsMap2, current, parent)

  val sizeLimit = 100000

  lazy val root              = "/" // Ugly but assume start at root
  val (_, fileSystemSizeMap) = filesystemToMaps(lines, Map.empty, Map.empty, root, None)
  val result1                = fileSystemSizeMap.filter(_._2 <= sizeLimit).values.sum

  println(s"Result pt. 1: $result1")

  // Part 2
  val totalUsedSpace = fileSystemSizeMap("/")
  val minSpaceToFree = 30000000 - (70000000 - totalUsedSpace)
  val result2 = fileSystemSizeMap.filter(_._2 >= minSpaceToFree).minBy(_._2)
  println(s"Result pt. 2: $result2")

  input.close()

}
