/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon

/**
  *
  */
package domain {

  case class GameMap(var x: Int, var y: Int, tiles: Array[Array[Tile]]) {

    private def indexAt(direction: Direction) = {
      direction match {
        case Up => (x, y-1)
        case Down => (x, y+1)
        case Left => (x-1, y)
        case Right => (x+1, y)
      }
    }

    private def tileAt(direction: Direction) = {
      indexAt(direction) match {
        case (-1, _) | (_, -1) => Wall
        case (x, y) if x == tiles.length || y == tiles(0).length => Wall
        case (x, y) => tiles(y)(x)
      }
    }

    private def tileAt(x: Int, y: Int): Tile = tiles(y)(x)

    private def setTile(x: Int, y: Int)(tile: Tile): Unit = tiles(y)(x) = tile

    private def player: Player = tileAt(x, y).asInstanceOf[Player]

    def describeLocation = getOrdinalTilesAt(getDirections)

    def playerLocation : WhereAmIResult = {
      WhereAmIResult(x,y)
    }

    def playerHealth : MyHealthResult = {
      player match {
        case Player(health) => MyHealthResult(health)
      }
    }

    def observe(direction: Direction) : ObserveResult = {
        ObserveResult(tileAt(direction), Seq(direction))
    }

    def move(direction: Direction): Seq[MoveResult] = {
      val (newX, newY) = indexAt(direction)

      tileAt(direction) match {
        case Blank =>
          setTile(newX, newY)(tileAt(x, y))
          setTile(x, y)(Blank)
          x = newX
          y = newY

          getOrdinalTilesAt(getDirections).map(Moved)

        case Wall => List(BumpWall)

        case Goal =>
          setTile(newX, newY)(tileAt(x, y))
          setTile(x, y)(Blank)
          x = newX
          y = newY

          List(Victory)

        case e: Enemy =>
          List(hurtPlayer(OrdinalEnemy(e, direction)))
      }
    }

    def attack(direction: Direction): AttackResult = {
      val (enemyX, enemyY) = indexAt(direction)

      tileAt(direction) match {
        case enemy @ Enemy(name, health) =>
          if (health == 1) {
            setTile(enemyX, enemyY)(Blank)
            Killed(OrdinalEnemy(enemy, direction))
          }
          else {
            setTile(enemyX, enemyY)(Enemy(name, health - 1))
            Hit(OrdinalEnemy(Enemy(name, health - 1), direction))
          }

        case _ => NothingThere
      }
    }

    def hurtPlayer(e : OrdinalEnemy) : CombatResult = {
      tiles(x)(y) = player.copy(health = player.health - 1)
      player.health match {
        case h if h < 1 => Dead(e)
        case h => Hurt(e, h)
      }
    }

    /**
      * Update nearby spaces.
      * Enemies only for now
      * @return
      */
    def updateEnemies(): Seq[CombatResult] = {
      getDirections.map { x => (tileAt(x), x) }
        .collect{case (e : Enemy, d) => hurtPlayer(OrdinalEnemy(e,d ))}
    }

    def getDirections : List[Direction] = {
      List(Up, Down, Left, Right)
    }

    def getOrdinalTilesAt(directions: List[Direction]) : List[OrdinalTile] = {
      directions match {
        case Nil => List()
        case x :: xs =>
          tileAt(x) match {
            case e: Enemy => OrdinalEnemy(e, x) :: getOrdinalTilesAt(xs)
            case t => OrdinalPlainTile(t, x) :: getOrdinalTilesAt(xs)
          }
      }
    }

  }

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  sealed trait Action

  sealed trait SystemAction extends Action
  case object Help extends SystemAction
  case object Stop extends SystemAction
  case object Cancel extends SystemAction

  sealed trait SamsAdventureIntent extends Action

  sealed trait UpdateAction extends SamsAdventureIntent
  case class Move(direction: Direction) extends UpdateAction
  case class Attack(direction: Direction) extends UpdateAction

  sealed trait InfoAction extends SamsAdventureIntent
  case class Observe(direction: Direction) extends InfoAction
  case object MyHealth extends InfoAction
  case object WhereAmI extends InfoAction
  case object DescribeSurroundings extends InfoAction
  case object BeScared extends InfoAction
  //case object Listen extends InfoAction

  case class PromptAction(waitState : String, promptResponse: PromptResponse) extends Action

  case object UnknownAction extends Action

  sealed trait Tile

  sealed trait TileAction
  sealed trait MoveTile extends TileAction
  sealed trait AttackableTile extends TileAction
  sealed trait BlockedTile extends TileAction
  sealed trait GoalTile extends MoveTile

  sealed trait TileImportance
  sealed trait ImportantTile extends TileImportance

  case object Blank extends Tile with MoveTile
  case object Wall extends Tile with BlockedTile
  case class Enemy(name: String, health: Int) extends Tile with AttackableTile with ImportantTile
  case class Player(health: Int) extends Tile
  case object Goal extends Tile with GoalTile with ImportantTile

  sealed trait OrdinalTile { def direction : Direction; def tile: Tile }
  case class OrdinalPlainTile(tile: Tile, direction: Direction) extends OrdinalTile
  case class OrdinalEnemy(tile: Enemy, direction: Direction) extends OrdinalTile

  case class ResultOrder(order: Int)

  sealed trait ActionResult{def order: Integer}

  sealed trait AttackResult extends ActionResult { override def order = 0}
  case object NothingThere extends AttackResult
  case class Killed(enemy: OrdinalEnemy) extends AttackResult
  case class Hit(enemy: OrdinalEnemy) extends AttackResult

  case class HurtHit(hurt: Hurt, hit: Hit) extends AttackResult
  case class MultiHurtHit(multiHurt: MultiHurt, hit: Hit) extends AttackResult

  sealed trait MoveResult extends ActionResult { override def order = 5}
  case object BumpWall extends MoveResult
  case object Victory extends MoveResult
  case class Moved(tile: OrdinalTile) extends MoveResult

  sealed trait CombatResult extends MoveResult
  case class Hurt(enemy: OrdinalEnemy, health: Int) extends CombatResult
  case class MultiHurt(enemy: OrdinalEnemy, health: Int, time: Int) extends CombatResult
  case class Dead(enemy: OrdinalEnemy) extends CombatResult

  sealed trait InfoResult extends ActionResult { override def order = 10}
  case class WhereAmIResult(x :Int, y : Int) extends InfoResult
  case class MyHealthResult(myHealth : Int) extends InfoResult
  case class ObserveResult(tile: Tile, directions: Seq[Direction]) extends InfoResult
  case class DescribeSurroundingsResult(ordinalTile: OrdinalTile) extends InfoResult
  case object IsScared extends InfoResult

  sealed trait SuggestionResult extends ActionResult { override def order = 100 }
  case class MoveSuggestion(direction: Direction) extends SuggestionResult
  case class MultiMoveSuggestion(direction: List[Direction]) extends SuggestionResult
  case class AttackSuggestion(direction: Direction) extends SuggestionResult
  case class MultiAttackSuggestion(direction: List[Direction]) extends SuggestionResult

  sealed trait SessionState
  case object EndSession extends SessionState
  case object OpenSession extends SessionState

  sealed trait GameResponse

  case class SessionUpdateResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput) extends GameResponse
  case class GameUpdateResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput, gameMap: GameMap)
    extends GameResponse
  case class GameInfoResponse(gameSpeechOutput: GameSpeechOutput) extends GameResponse

  sealed trait PromptUpdateResponse

  case class PromptFinishedResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput) extends PromptUpdateResponse
  case class PromptOpenResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput, waitPromptState: String) extends PromptUpdateResponse

  sealed trait GameSpeechOutput

  case class SSMLOutput(message: String) extends GameSpeechOutput
  case class PlainTextOutput(message: String) extends GameSpeechOutput

  sealed trait PromptResponse

  case object Yes extends PromptResponse
  case object No extends PromptResponse
  case object Unknown extends PromptResponse

}
