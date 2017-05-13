package com.amazon.hackathon

import com.amazon.hackathon.domain.{TrivialTile, _}

object TextEngine {
  def resolveResultText(result : ActionResult) : String = {
    result match {
      case NothingThere => "There is nothing to attack"

      case Killed(Enemy(name, _)) => s"You killed a $name"

      case Hit(Enemy(name, health)) => s"You hit the $name, it now has $health health"

      case BumpWall => "You walked into a wall dummy."

      case Moved(directions) => locationMessage(ignoreTrivial(ordinalGroupByTile(directions)))

      case Hurt(Enemy(name, enemyHealth), health) => s"you were hurt by a $name. You now have $health health"

      case Dead(Enemy(name, eh)) => s"you are dead, Killed by a $name. It stares down at your lifeless body, laughing."

      case Victory => "You found the stairs down to the next dungeon. Unfortunately the ghouls are still building it. Come back soon."

      case MyHealthResult(health) => s"You have $health"

      case WhereAmIResult(x,y) => s"You are at position $x, $y"

      case ObserveResult(tile) => tileMessage(tile)

      case DescribeSurroundingsResult(directions) => locationMessage(ordinalGroupByTile(directions))
    }
  }

  def ordinalGroupByTile(directions: Ordinal) : Seq[(Tile, Seq[String])] = {
    val map = directions match {
      case Ordinal(left,right,up,down) => Map ("west" -> left, "east" -> right, "north" -> up, "south" -> down)
    }

    map.toSeq.groupBy(_._2)
      .map { case (tile, group) => tile -> group.map(_._1) }.toSeq
  }

  def ignoreTrivial( tiles : Seq[(Tile, Seq[String])]) : Seq[(Tile, Seq[String])] = {
    tiles.filter{x => x._1 match {
      case _ : TrivialTile => false
      case _ => true }}
  }

  def locationMessage(directions:  Seq[(Tile, Seq[String])]): String = {
    directions.map { x => tileMessage(x._1) + " is to the " + directionText(x._2.toList) }.mkString(". ")
  }

  def directionText(directions: List[String]): String = {
    directions match {
      case Nil => ""
      case x :: Nil => x
      case x :: y :: Nil => x + " and " + y
      case x :: xs => x + " " + directionText(xs)
    }
  }

  def resolveResults(results: Seq[ActionResult]) : String ={
    results.toList match {
      case Nil => ""
      case x :: Nil => resolveResultText(x)
      case (x:Dead) :: y => resolveResultText(x)
      case x :: xs => resolveResultText(x) + ". " + resolveResults(xs)
    }
  }

  private def tileMessage(tile: Tile): String = tile match {
    case Blank => "empty space"
    case Wall => s"a wall"
    case Goal => "the stairs down"
    case Enemy(name, health) => s"a $name with $health health"
    case _ => "oopsy-daisy"
  }
}
