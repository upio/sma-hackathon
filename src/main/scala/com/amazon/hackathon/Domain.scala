/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon

/**
  *
  */
package domain {

  import com.amazon.hackathon.SamsAdventureSpeechlet.Direction

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

    def describeLocation = Moved(tileAt(Left), tileAt(Right), tileAt(Up), tileAt(Down))

    def move(direction: Direction): MoveResult = {
      val (newX, newY) = indexAt(direction)

      tileAt(direction) match {
        case Blank =>
          setTile(newX, newY)(tileAt(x, y))
          setTile(x, y)(Blank)
          x = newX
          y = newY

          Moved(tileAt(Left), tileAt(Right), tileAt(Up), tileAt(Down))

        case Wall => BumpWall

        case Goal =>
          setTile(newX, newY)(tileAt(x, y))
          setTile(x, y)(Blank)
          x = newX
          y = newY

          Victory

        case e: Enemy =>
          tiles(x)(y) = player.copy(health = player.health - 1)
          Hurt(e, player.health)
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
  case class Move(direction: Direction) extends SamsAdventureIntent
  case class Attack(direction: Direction) extends SamsAdventureIntent

  sealed trait Tile
  case object Blank extends Tile
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
  case class Moved(left: Tile, right: Tile, up: Tile, down: Tile) extends MoveResult
  case class Hurt(enemy: Enemy, health: Int) extends MoveResult
}
