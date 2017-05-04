/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon


import com.amazon.hackathon.domain._
import com.amazon.speech.speechlet._

import scala.collection.JavaConverters._
import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler
import com.amazon.speech.ui.{OutputSpeech, PlainTextOutputSpeech, Reprompt}
import spray.json._
import fommil.sjs.FamilyFormats._

object SamsAdventureStreamHandler extends SpeechletRequestStreamHandler(SamsAdventureSpeechlet, Set("").asJava)

object SamsAdventureSpeechlet extends Speechlet {

  val baseMap = GameMap(2, 3, Array(
    Array(Wall, Wall, Wall, Blank, Goal),
    Array(Wall, Wall, Enemy("ghoul", 2), Blank, Wall),
    Array(Blank, Blank, Blank, Wall, Wall),
    Array(Blank, Blank, Player(10), Blank, Blank),
    Array(Blank, Blank, Wall, Blank, Wall)
  ))

  def onSessionEnded(request: SessionEndedRequest, session: Session): Unit = ???

  private def getAction(request: IntentRequest): SamsAdventureIntent = ???

  def onIntent(request: IntentRequest, session: Session): SpeechletResponse = {
    var intent = request.getIntent

    var direction = getDirectionFromSlot(intent.getSlots.keySet().asScala.toSet)

    var action = intent.getName match {
      case "moveIntent" => Move
      case "attackIntent" => Attack
      case "AMAZON.StopIntent" => Stop
      case "AMAZON.HelpIntent" => Help
      case "AMAZON.CancelIntent" => Cancel
    }

    action match {
      case x : SystemAction => handleSystemAction(x)
      case x : SamsAdventureIntent => handleGameAction(x, session, request)
    }
  }

  def onLaunch(request: LaunchRequest, session: Session): SpeechletResponse = ???

  def onSessionStarted(request: SessionStartedRequest, session: Session): Unit = {
    session.setAttribute("map", baseMap.toJson.prettyPrint)
  }

  def getDirectionFromSlot(slots: Set[String]) : Direction = {
    slots match {
      case x if x.contains("leftDirection") => Left
      case x if x.contains("rightDirection") => Right
      case x if x.contains("upDirection") => Up
      case x if x.contains("downDirection") => Down
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

    val result = getAction(request) match {
      case Move(direction) => gameMap.move(direction)
      case Attack(direction) => gameMap.attack(direction)
    }

    session.setAttribute("map", gameMap.toJson.prettyPrint)

    def tileMessage(tile: Tile): String = tile match {
      case Blank | Wall  => s"a ${tile.toString}"
      case Goal => "the goal!"
      case Enemy(name, health) => s"a $name with $health health"
      case _ => "oopsy-daisy"
    }

    val message = result match {
      case NothingThere => "There is nothing to attack"

      case Killed(Enemy(name, _)) => s"You killed a $name"

      case Hit(Enemy(name, health)) => s"You hit the $name, it now has $health health"

      case BumpWall => "You walked into a wall, dummy."

      case Moved(left, right, up, down) =>
        s"to the left of you is ${tileMessage(left)}." +
          s"to the right of you is ${tileMessage(right)}" +
          s"above you is ${tileMessage(right)}" +
          s"below you is ${tileMessage(right)}"

      case Hurt(Enemy(name, enemyHealth), health) => s"you were hurt by a $enemyHealth. You now have $health health"

      case Victory => "You win!"
    }

    val response = new SpeechletResponse()
    val speech = new PlainTextOutputSpeech()
    speech.setText(message)
    response.setOutputSpeech(speech)

    response
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
}