/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon


import com.amazon.speech.speechlet._

import scala.collection.JavaConverters._
import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler

import scala.util.{Right => Good, Left => Bad}

/**
  *
  */
object SamsAdventureSpeechlet extends Speechlet {

  val baseMap = GameMap((1, 2), Array(
    Array(Wall, Wall, Wall, Blank, Goal),
    Array(Wall, Wall, Enemy("ghoul", 2), Blank, Wall),
    Array(Blank, Blank, Blank, Wall, Wall),
    Array(Blank, Blank, Player(10), Blank, Blank),
    Array(Blank, Blank, Wall, Blank, Wall)
  ))

  def onSessionEnded(request: SessionEndedRequest, session: Session): Unit = ???

  def onIntent(request: IntentRequest, session: Session): SpeechletResponse = {
    request.getIntent

    ???
  }

  def onLaunch(request: LaunchRequest, session: Session): SpeechletResponse = ???

  def onSessionStarted(request: SessionStartedRequest, session: Session): Unit = ???
}

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

sealed trait SamsAdventureIntent
case class Move(direction: Direction) extends SamsAdventureIntent
case class Attack(direction: Direction) extends SamsAdventureIntent

sealed trait Tile
case object Blank extends Tile
case object Wall extends Tile
case class Enemy(name: String, health: Int) extends Tile
case class Player(health: Int) extends Tile
case object Goal extends Tile

case class GameMap(playerIndex: (Int, Int), tiles: Array[Array[Tile]]) {
  val (x, y) = playerIndex
  private def indexAt(direction: Direction) = direction match {
    case Up => (x, y+1)
    case Down => (x, y-1)
    case Left => (x-1, y+1)
    case Right => (x+1, y+1)
  }

  def move(direction: Direction): Either[String, GameMap] = {
    val (newX, newY) = indexAt(direction)

    tiles(x)(y) match {
      case Blank =>
        tiles(newX)(newY) = tiles(x)(y)
        tiles(x)(y) = Blank

        Good(GameMap((newX, newY), tiles))

      case Wall => Bad(s"there is a wall $direction there")

      case Goal => Bad("you found the goal! You win!")

      case Enemy(name, health) => Bad(s"you can't move, a $name with $health health blocks your path")
    }
  }

  def attack(direction: Direction): Either[String, GameMap] = {
    val (enemyX, enemyY) = indexAt(direction)

//    tiles(x)(y) match {
//      case Enemy(name, health) => null
//    }

    ???
  }

}




object SamsAdventureStreamHandler extends SpeechletRequestStreamHandler(SamsAdventureSpeechlet, Set("").asJava)

