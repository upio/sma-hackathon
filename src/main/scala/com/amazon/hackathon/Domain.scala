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

    def describeLocation = Ordinal(tileAt(Left), tileAt(Right), tileAt(Up), tileAt(Down))

    def playerLocation : WhereAmIResult = {
      WhereAmIResult(x,y)
    }

    def playerHealth : MyHealthResult = {
      player match {
        case Player(health) => MyHealthResult(health)
      }
    }

    def observe(direction: Direction) : ObserveResult = {
        ObserveResult(tileAt(direction))
    }

    def move(direction: Direction): MoveResult = {
      val (newX, newY) = indexAt(direction)

      tileAt(direction) match {
        case Blank =>
          setTile(newX, newY)(tileAt(x, y))
          setTile(x, y)(Blank)
          x = newX
          y = newY

          Moved(Ordinal(tileAt(Left), tileAt(Right), tileAt(Up), tileAt(Down)))

        case Wall => BumpWall

        case Goal =>
          setTile(newX, newY)(tileAt(x, y))
          setTile(x, y)(Blank)
          x = newX
          y = newY

          Victory

        case e: Enemy =>
          hurtPlayer(e)
      }
    }

    def attack(direction: Direction): AttackResult = {
      val (enemyX, enemyY) = indexAt(direction)

      tileAt(direction) match {
        case enemy @ Enemy(name, health) =>
          if (health == 1) {
            setTile(enemyX, enemyY)(Blank)
            Killed(enemy)
          }
          else {
            setTile(enemyX, enemyY)(Enemy(name, health - 1))
            Hit(Enemy(name, health - 1))
          }

        case _ => NothingThere
      }
    }

    def hurtPlayer(e : Enemy) : CombatResult = {
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
      getDirections.map { tileAt }
        .collect{case e : Enemy => hurtPlayer(e)}
    }

    def getDirections : Seq[Direction] = {
      List(Up, Down, Left, Right)
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
  //case object Listen extends InfoAction

  sealed trait Tile

  sealed trait TrivialTile extends Tile

  case object Blank extends TrivialTile
  case object Wall extends Tile
  case class Enemy(name: String, health: Int) extends Tile
  case class Player(health: Int) extends Tile
  case object Goal extends Tile

  sealed trait ActionResult

  sealed trait AttackResult extends ActionResult
  case object NothingThere extends AttackResult
  case class Killed(enemy: Enemy) extends AttackResult
  case class Hit(enemy: Enemy) extends AttackResult

  sealed trait MoveResult extends ActionResult
  case object BumpWall extends MoveResult
  case object Victory extends MoveResult
  case class Moved(directions: Ordinal) extends MoveResult

  sealed trait CombatResult extends MoveResult
  case class Hurt(enemy: Enemy, health: Int) extends CombatResult
  case class Dead(enemy: Enemy) extends CombatResult

  sealed trait InfoResult extends ActionResult
  case class WhereAmIResult(x :Int, y : Int) extends InfoResult
  case class MyHealthResult(myHealth : Int) extends InfoResult
  case class ObserveResult(tile: Tile) extends InfoResult
  case class DescribeSurroundingsResult(ordinal: Ordinal) extends InfoResult

  case class Ordinal(left: Tile, right: Tile, up: Tile, down: Tile)

  sealed trait SessionState
  case object EndSession extends SessionState
  case object OpenSession extends SessionState

  sealed trait GameResponse

  case class SessionUpdateResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput) extends GameResponse
  case class GameUpdateResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput, gameMap: GameMap)
    extends GameResponse
  case class GameInfoResponse(gameSpeechOutput: GameSpeechOutput) extends GameResponse

  sealed trait GameSpeechOutput

  case class SSMLOutput(message: String) extends GameSpeechOutput
  case class PlainTextOutput(message: String) extends GameSpeechOutput

}
