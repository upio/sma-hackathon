package com.amazon.hackathon

import com.amazon.hackathon.domain.{TrivialTile, _}

object TextEngine {
  def resolveResultText(result: ActionResult): String = {
    result match {
      case NothingThere => "There is nothing to attack"

      case Killed(Enemy(name, _)) => s"You killed a $name"

      case Hit(Enemy(name, health)) => s"You hit the $name, it now has $health health"

      case BumpWall => "You walked into a wall dummy. You can ask for me what my sensors see."

      case Moved(directions) => locationMessage(ignoreTrivial(ordinalGroupByTile(directions)))

      case Hurt(Enemy(name, enemyHealth), health) => s"a $name hits you"

      case MultiHurt(enemy, health, times) => resolveResultText(Hurt(enemy, health)) + " " + timesString(times)

      case MultiHurtHit(MultiHurt(Enemy(name, _), _, times), _) => s"You strike the $name and it hits you back " + timesString(times)
      case HurtHit(Hurt(Enemy(name, _), _), _) => s"You strike the $name and it hits you back "

      case Dead(Enemy(name, eh)) => s"you are dead, Killed by a $name. It stares down at your lifeless body, laughing."

      case Victory => "We found the stairs down to the next dungeon. Unfortunately it appears that ghouls are still building it. Lets try again later"

      case MyHealthResult(health) => s"You have $health"

      case WhereAmIResult(x, y) => s"We are at position $x, $y. Whatever that means."

      case ObserveResult(tile) => tileMessage(tile)

      case DescribeSurroundingsResult(directions) => locationMessage(ordinalGroupByTile(directions))

      case IsScared => "Don't worry hero, we'll get through this. Remember you can always ask me to describe your surroundings or ask for help. Would you like a hug?"
    }
  }

  def timesString(times: Int) = {
    times match {
      case x if x <= 1 => ""
      case 2 => "twice"
      case 3 => "thrice"
      case _ => "many times"
    }
  }

  def ordinalGroupByTile(directions: Ordinal): Seq[(Tile, Seq[String])] = {
    val map = directions match {
      case Ordinal(left, right, up, down) => Map("west" -> left, "east" -> right, "north" -> up, "south" -> down)
    }

    map.toSeq.groupBy(_._2)
      .map { case (tile, group) => tile -> group.map(_._1) }.toSeq
  }

  def ignoreTrivial(tiles: Seq[(Tile, Seq[String])]): Seq[(Tile, Seq[String])] = {
    tiles.filter { x =>
      x._1 match {
        case _: TrivialTile => false
        case _ => true
      }
    }
  }

  def locationMessage(directions: Seq[(Tile, Seq[String])]): String = {
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

  def resolveResults(results: Seq[ActionResult]): String = {
    reduceResults(results.toList) match {
      case Nil => ""
      case x :: Nil => resolveResultText(x)
      case (x: Dead) :: _ => resolveResultText(x)
      case x :: xs => resolveResultText(x) + ". " + resolveResults(xs)
    }
  }

  def reduceResults(results: List[ActionResult]): List[ActionResult] = {
    List(HurtRule, HitRule, HurtHitRule)
      .foldLeft(results) { (x, y) => y.action(x) }
  }

  private def tileMessage(tile: Tile): String = tile match {
    case Blank => "empty space"
    case Wall => s"a wall"
    case Goal => "the stairs down"
    case Enemy(name, health) => s"a $name with $health health"
    case _ => "oopsy-daisy"
  }

  sealed trait ReduceRule {
    def action(actions: List[ActionResult]): List[ActionResult]
  }

  case object EverythingElse

  case object HurtRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] =
      actions.groupBy {
        case Hurt(e, _) => e
        case _ => EverythingElse
      }
        .flatMap(x => x._1 match {
          case e: Enemy => List(MultiHurt(e, x._2.asInstanceOf[List[Hurt]].map(_.health).min, x._2.length))
          case EverythingElse => x._2
        })
        .toList
  }

  case object HitRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] =
      actions.groupBy {
        case Hit(e) => e
        case _ => EverythingElse
      }
        .flatMap(x => x._1 match {
          case e: Enemy => List(Hit(e))
          case EverythingElse => x._2
        })
        .toList
  }

  case object HurtHitRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] =
      actions.groupBy {
        case Hit(e) => e
        case Hurt(e, _) => e
        case MultiHurt(e, _, _) => e
        case _ => EverythingElse
      }
        .flatMap(x => x._1 match {
          case _: Enemy => x._2 match {
            case (h: Hit) :: (hu: Hurt) :: Nil => List(HurtHit(hu, h))
            case (hu: Hurt) :: (h: Hit) :: Nil => List(HurtHit(hu, h))
            case (h: Hit) :: (hu: MultiHurt) :: Nil => List(MultiHurtHit(hu, h))
            case (hu: MultiHurt) :: (h: Hit) :: Nil => List(MultiHurtHit(hu, h))
            case xs => xs
          }
          case EverythingElse => x._2
        })
        .toList
  }

}