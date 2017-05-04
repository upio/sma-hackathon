/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon


import com.amazon.hackathon.domain._
import com.amazon.speech.slu.Slot
import com.amazon.speech.speechlet._

import scala.collection.JavaConverters._
import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler
import com.amazon.speech.ui.{OutputSpeech, PlainTextOutputSpeech, Reprompt}
import spray.json._
import fommil.sjs.FamilyFormats._

class SamsAdventureStreamHandler extends SpeechletRequestStreamHandler(SamsAdventureSpeechlet, Set("amzn1.ask.skill.58a1303c-ee45-485b-a01d-e626a1713fa7").asJava)

object SamsAdventureSpeechlet extends Speechlet {

  val baseMap = GameMap(2, 3, Array(
    Array(Wall, Wall, Wall, Blank, Goal),
    Array(Wall, Wall, Enemy("ghoul", 2), Blank, Wall),
    Array(Blank, Blank, Blank, Wall, Wall),
    Array(Blank, Blank, Player(10), Blank, Blank),
    Array(Blank, Blank, Wall, Blank, Wall)
  ))

  def onSessionEnded(request: SessionEndedRequest, session: Session): Unit = ???

  private def tileMessage(tile: Tile): String = tile match {
    case Blank | Wall  => s"a ${tile.toString}"
    case Goal => "the goal!"
    case Enemy(name, health) => s"a $name with $health health"
    case _ => "oopsy-daisy"
  }

  def onIntent(request: IntentRequest, session: Session): SpeechletResponse = {
    println(request.getIntent.getName)
    println(request.getIntent.getSlots.asScala)

    val intent = request.getIntent

    val direction = getDirectionFromSlot(intent.getSlots.asScala.toMap)

    val action = intent.getName match {
      case "moveIntent" => Move(direction)
      case "attackIntent" => Attack(direction)
      case "AMAZON.StopIntent" => Stop
      case "AMAZON.HelpIntent" => Help
      case "AMAZON.CancelIntent" => Cancel
    }

    action match {
      case x : SystemAction => handleSystemAction(x)
      case x : SamsAdventureIntent => handleGameAction(x, session, request)
    }
  }

  def getDirectionFromSlot(slots: Map[String, Slot]) : Direction = {
    println(slots)
    slots.values.find(_.getValue != null).get.getName match {
      case "leftDirection" => Left
      case "rightDirection" => Right
      case "upDirection" => Up
      case "downDirection" => Down
    }
  }

  def handleSystemAction(action: SystemAction): SpeechletResponse  = {
    action match {
      case Help =>
        val out = new PlainTextOutputSpeech
        out.setText("Help me, help you.")
        SpeechletResponse.newAskResponse(out, new Reprompt)
      case Stop =>
        val out = new PlainTextOutputSpeech
        out.setText("Come back soon.")
        val s = SpeechletResponse.newAskResponse(out, new Reprompt)
        s.setShouldEndSession(true)
        s
      case Cancel =>
        val out = new PlainTextOutputSpeech
        out.setText("Cancel?. Just keep playing.")
        SpeechletResponse.newAskResponse(out, new Reprompt)
    }
  }

  def handleGameAction(action: SamsAdventureIntent, session: Session, request: IntentRequest): SpeechletResponse = {
    val gameMap = session.getAttribute("map").asInstanceOf[String].parseJson.convertTo[GameMap]

    val result = action match {
      case Move(direction) => gameMap.move(direction)
      case Attack(direction) => gameMap.attack(direction)
    }

    session.setAttribute("map", gameMap.toJson.compactPrint)

    val message = result match {
      case NothingThere => "There is nothing to attack"

      case Killed(Enemy(name, _)) => s"You killed a $name"

      case Hit(Enemy(name, health)) => s"You hit the $name, it now has $health health"

      case BumpWall => "You walked into a wall, dummy."

      case Moved(left, right, up, down) =>
        s"to the left of you is ${tileMessage(left)}. " +
        s"to the right of you is ${tileMessage(right)}. " +
        s"above you is ${tileMessage(right)}. " +
        s"below you is ${tileMessage(right)}. "

      case Hurt(Enemy(name, enemyHealth), health) => s"you were hurt by a $name. You now have $health health"

      case Victory => "You win!"
    }

    val response = new SpeechletResponse()
    response.setShouldEndSession(if (message == "You win!") true else false)
    val speech = new PlainTextOutputSpeech()
    speech.setText(message)
    response.setOutputSpeech(speech)

    response
  }

  def onLaunch(request: LaunchRequest, session: Session): SpeechletResponse = {
    session.setAttribute("map", baseMap.toJson.compactPrint)

    val welcome = baseMap.describeLocation match {
      case Moved(left, right, up, down) =>
        "Welcome to Sam's Adventure!" +
        s"to the left of you is ${tileMessage(left)}. " +
        s"to the right of you is ${tileMessage(right)}. " +
        s"above you is ${tileMessage(right)}. " +
        s"below you is ${tileMessage(right)}. "
    }

    val response = new SpeechletResponse()
    val speech = new PlainTextOutputSpeech()
    speech.setText(welcome)
    response.setOutputSpeech(speech)
    response.setShouldEndSession(false)
    response
  }

  def onSessionStarted(request: SessionStartedRequest, session: Session): Unit = {
  }
}

object Scratch extends App {
  val map = GameMap(2, 3, Array(
    Array(Wall, Wall, Wall, Blank, Goal),
    Array(Wall, Wall, Enemy("ghoul", 2), Blank, Wall),
    Array(Blank, Blank, Blank, Wall, Wall),
    Array(Blank, Blank, Player(10), Blank, Blank),
    Array(Blank, Blank, Wall, Blank, Wall)
  ))

  def printAndMap(result: ActionResult): Unit = {
    println(result)
    map.tiles.map(_.toList).foreach(println)
    println("-----------------------------------------")
  }

  printAndMap(map.move(Up))
  printAndMap(map.attack(Up))
  printAndMap(map.attack(Up))
  printAndMap(map.move(Right))
  printAndMap(map.move(Up))
  printAndMap(map.move(Right))
  printAndMap(map.move(Right))
  printAndMap(map.move(Up))
  printAndMap(map.move(Right))
  printAndMap(map.move(Right))
  printAndMap(map.move(Right))

  println(map.toJson.compactPrint)
  println(map.toJson.compactPrint.size)
}