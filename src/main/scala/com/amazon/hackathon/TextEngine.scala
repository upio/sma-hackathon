package com.amazon.hackathon

import com.amazon.hackathon.domain.{ActionResult, AttackSuggestion, Blank, BumpWall, Dead, DescribeSurroundingsResult, Direction, Down, Enemy, Goal, GoalTile, Hit, Hurt, HurtHit, ImportantTile, IsScared, Killed, Left, MoveSuggestion, MoveTile, Moved, MultiAttackSuggestion, MultiHurt, MultiHurtHit, MultiMoveSuggestion, MyHealthResult, NothingThere, ObserveResult, OrdinalEnemy, OrdinalPlainTile, Right, SuggestionResult, Tile, Up, Victory, Wall, WhereAmIResult}


object TextEngine {
  def resolveResultText(result: ActionResult): String = {
    result match {
      case NothingThere => "There is nothing to attack"

      case Killed(OrdinalEnemy(Enemy(name, _), d)) => s"You killed a $name to the ${directionString(d)}"

      case Hit(OrdinalEnemy(Enemy(name, health), _)) => s"You hit the $name, it now has $health health"

      case BumpWall => "You walked into a wall dummy. You can ask for me what my sensors see."

      case Hurt(OrdinalEnemy(Enemy(name, _), d), _) => s"a $name to the ${directionString(d)} hits you"

      case MultiHurt(enemy, health, times) => resolveResultText(Hurt(enemy, health)) + " " + timesString(times)

      case MultiHurtHit(MultiHurt(OrdinalEnemy(Enemy(name, _), _), _, times), _) => s"You strike the $name and it hits you back " + timesString(times)
      case HurtHit(Hurt(OrdinalEnemy(Enemy(name, _), _), _), _) => s"You strike the $name and it hits you back "

      case Dead(OrdinalEnemy(Enemy(name, _), d)) => s"you are dead, Killed by a $name to the ${directionString(d)}. It stares down at your lifeless body, laughing."

      case Victory => "We found the stairs down to the next dungeon. Unfortunately it appears that the ghouls are still building it. Lets try again later"

      case MyHealthResult(health) => s"You have $health"

      case WhereAmIResult(x, y) => s"We are at position $x, $y. Whatever that means."

      case ObserveResult(tile, directions) => locationMessage(tile, directions)

      case DescribeSurroundingsResult(ordinalTile) => locationMessage(ordinalTile.tile, Seq(ordinalTile.direction))

      case Moved(ordinalTile) => locationMessage(ordinalTile.tile, Seq(ordinalTile.direction))

      case IsScared => "Don't worry hero, we'll get through this. Remember you can always ask me to describe your surroundings or ask for help. Would you like a hug?"

      case MoveSuggestion(d) => "We can only go " + directionString(d)
      case MultiMoveSuggestion(ds) => "We can go " + directionText(ds, separator = ", ", conjuction = "or")
      case AttackSuggestion(d) => "You should attack " + directionString(d) + ", hero"
      case MultiAttackSuggestion(ds) => "You should attack " + directionText(ds, separator = ", ", conjuction = "or") + ", hero"
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

  def directionString(direction: Direction) = {
    direction match {
      case Up => "north"
      case Down => "south"
      case Left => "west"
      case Right => "east"
    }
  }

  def locationMessage(tile: Tile, directions: Seq[Direction]): String = {
    tileMessage(tile) + " is to the " + directionText(directions.toList)
  }

  def directionText(directions: List[Direction], separator: String = " ", conjuction: String = "and"): String = {
    directions match {
      case Nil => ""
      case x :: Nil => directionString(x)
      case x :: y :: Nil => directionString(x) + separator + conjuction + " " + directionString(y)
      case x :: xs => directionString(x) + separator + directionText(xs, separator = separator, conjuction = conjuction)
    }
  }

  def resolveResults(results: Seq[ActionResult]): String = {
    println("Before reducing")
    println(results)
    val reduced = reduceResults(results.toList).sortBy(_.order)
    println("After reducing")
    println(reduced)
    reduced match {
      case Nil => ""
      case x :: Nil => resolveResultText(x)
      case (x: Dead) :: _ => resolveResultText(x)
      case x :: xs => resolveResultText(x) + ". " + resolveResults(xs)
    }
  }

  def reduceResults(results: List[ActionResult]): List[ActionResult] = {
    List(
      SuggestionGeneration, //generate next move suggestions based on the results
      HurtRule, //one hurt result per turn by enemy
      HitRule, //join all hits by enemy
      HurtHitRule, //if hit and hurt by same enemy, join
      HurtToObserveRule,
      SuggestionsPriorityRule, // if there is an attack suggestion, filter out move suggestion
      ReduceSuggestionsRule, // if there are multiple of the same suggestion type, reduce
      MoveObserveImportanceRule,
      ObserveLikeTilesRule //join all remaining tile observations by tile type
    ).foldLeft(results) { (x, y) => y.action(x) }
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
          case e: OrdinalEnemy =>
            List(x._2.asInstanceOf[List[Hurt]] match {
              case (x: Hurt) :: Nil => x
              case xs => MultiHurt(e, xs.map(_.health).min, xs.length)
            })
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
          case e: OrdinalEnemy => List(Hit(e))
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
          case _: OrdinalEnemy => x._2 match {
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

  case object SuggestionGeneration extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] = {
      def suggestions(a: List[ActionResult]): List[SuggestionResult] = a match {
        case Nil => List()
        case Moved(OrdinalPlainTile(_: GoalTile, d)) :: xs => MoveSuggestion(d) +: suggestions(xs)
        case Moved(OrdinalPlainTile(_: MoveTile, d)) :: xs => MoveSuggestion(d) +: suggestions(xs)
        case Moved(OrdinalEnemy(_, d)) :: xs => AttackSuggestion(d) +: suggestions(xs)
        case DescribeSurroundingsResult(OrdinalPlainTile(_: GoalTile, d)) :: xs => MoveSuggestion(d) +: suggestions(xs)
        case DescribeSurroundingsResult(OrdinalPlainTile(_: MoveTile, d)) :: xs => MoveSuggestion(d) +: suggestions(xs)
        case DescribeSurroundingsResult(OrdinalEnemy(_, d)) :: xs => AttackSuggestion(d) +: suggestions(xs)
        case _ :: xs => suggestions(xs)
      }

      suggestions(actions) ++ actions
    }
  }

  /**
    * If there is an attack suggestion, do not present move suggestions
    */
  case object SuggestionsPriorityRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] = {
      val (suggestions, remaining) = actions.partition {
        case _: SuggestionResult => true
        case _ => false
      }
      remaining ++
        (if (suggestions.exists {
          case _: AttackSuggestion => true
          case _ => false
        }) suggestions.filter {
          case _: AttackSuggestion => true
          case _ => false
        } else suggestions)
    }
  }

  case object ReduceSuggestionsRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] =
      actions.groupBy {
        case _: MoveSuggestion => MoveSuggestion
        case _: AttackSuggestion => AttackSuggestion
        case _ => EverythingElse
      }
        .flatMap(x => x._1 match {
          case EverythingElse => x._2
          case _ if x._2.length == 1 => x._2
          case MoveSuggestion => List(MultiMoveSuggestion(x._2.asInstanceOf[List[MoveSuggestion]].map(_.direction)))
          case AttackSuggestion => List(MultiAttackSuggestion(x._2.asInstanceOf[List[AttackSuggestion]].map(_.direction)))
        })
        .toList
  }

  /**
    * Only describe important tiles on move action
    */
  case object MoveObserveImportanceRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] = {
      def r(a: List[ActionResult]): List[ActionResult] = {
        a match {
          case Nil => List()
          case (x: Moved) :: xs => x match {
            case Moved(OrdinalPlainTile(_: ImportantTile, _)) | Moved(OrdinalEnemy(_: ImportantTile, _)) => x +: r(xs)
            case _ => r(xs)
          }
          case x :: xs => x +: r(xs)
        }
      }

      r(actions)
    }
  }

  case object ObserveLikeTilesRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] =
      actions.groupBy {
        case Moved(OrdinalPlainTile(t, _)) => t
        case Moved(OrdinalEnemy(e, _)) => e
        case DescribeSurroundingsResult(OrdinalPlainTile(t, _)) => t
        case DescribeSurroundingsResult(OrdinalEnemy(e, _)) => e
        case _ => EverythingElse
      }.flatMap(x => x._1 match {
        case t: Tile => List(ObserveResult(t, x._2.map {
          case Moved(o) => o.direction
          case DescribeSurroundingsResult(o) => o.direction
        }))
        case EverythingElse => x._2
      }).toList
  }

  case object HurtToObserveRule extends ReduceRule {
    override def action(actions: List[ActionResult]): List[ActionResult] =
      actions.groupBy {
        case Moved(e: OrdinalEnemy) => e
        case Hurt(e: OrdinalEnemy, _) => e
        case HurtHit(_, Hit(e: OrdinalEnemy)) => e
        case MultiHurt(e: OrdinalEnemy, _, _) => e
        case MultiHurtHit(_, Hit(e: OrdinalEnemy)) => e
        case _ => EverythingElse
      }.flatMap(x => x._1 match {
        case EverythingElse => x._2
        case _ =>
          if (x._2.exists {
            case (_: Hurt | _: HurtHit | _: MultiHurt | _: MultiHurtHit) => true
            case _ => false
          })
            x._2.filter {
              case _: Moved => false
              case _ => true
            } else x._2
      }).toList
  }

}