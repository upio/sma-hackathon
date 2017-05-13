/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon


import com.amazon.hackathon.domain._
import com.amazon.speech.slu.Slot
import com.amazon.speech.speechlet._

import scala.collection.JavaConverters._
import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler
import com.amazon.speech.ui.{PlainTextOutputSpeech, Reprompt, SsmlOutputSpeech, OutputSpeech}
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

  def onSessionEnded(request: SessionEndedRequest, session: Session): Unit = {

  }

  def onIntent(request: IntentRequest, session: Session): SpeechletResponse = {
    println(request.getIntent.getName)
    println(request.getIntent.getSlots.asScala)

    val intent = request.getIntent

    val action = intent.getName match {
      case "moveIntent" => Move(getDirectionFromSlot(intent.getSlots.asScala.toMap))
      case "attackIntent" => Attack(getDirectionFromSlot(intent.getSlots.asScala.toMap))
      case "observeIntent" => Observe(getDirectionFromSlot(intent.getSlots.asScala.toMap))
      case "whereAmIIntent" => WhereAmI
      case "describeSurroundingsIntent" => DescribeSurroundings
      case "myHealthIntent" => MyHealth
      case "AMAZON.StopIntent" => Stop
      case "AMAZON.HelpIntent" => Help
      case "AMAZON.CancelIntent" => Cancel
    }

    val result = action match {
      case x: SystemAction => handleSystemAction(x)
      case x: SamsAdventureIntent => handleGameAction(x, session, request)
    }

    buildResponse(result)
  }

  def buildResponse(gameResponse: GameResponse) : SpeechletResponse = {
    gameResponse match {
      case SessionUpdateResponse(sessionState, gameSpeechOutput) =>
        getSpeechletResponse(sessionState, gameSpeechOutput)
      case GameUpdateResponse(sessionState, gameSpeechOutput, _) =>
        getSpeechletResponse(sessionState, gameSpeechOutput)
      case GameInfoResponse(gameSpeechOutput) =>
        getSpeechletResponse(OpenSession, gameSpeechOutput)
    }
  }

  def getSpeechletResponse(sessionState: SessionState, gameSpeechOutput: GameSpeechOutput) : SpeechletResponse = {
    new SpeechletResponse(){
      setShouldEndSession(sessionState match {
        case EndSession => true
        case OpenSession => false
      })
      setOutputSpeech(getSpeechOutput(gameSpeechOutput))
    }
  }

  def getSpeechOutput(gameSpeechOutput: GameSpeechOutput) : OutputSpeech = {
    gameSpeechOutput match {
      case PlainTextOutput(message) =>
        new PlainTextOutputSpeech() { setText(message)}
      case SSMLOutput(message) =>
        new SsmlOutputSpeech(){ setSsml(message) }
    }
  }

  def getDirectionFromSlot(slots: Map[String, Slot]): Direction = {
    println(slots)
    slots.values.find(_.getValue != null).get.getName match {
      case "leftDirection" => Left
      case "rightDirection" => Right
      case "upDirection" => Up
      case "downDirection" => Down
    }
  }

  def handleSystemAction(action: SystemAction): GameResponse = {
    action match {
      case Help =>
        GameInfoResponse(PlainTextOutput("Help me, help you."))
      case Stop =>
        SessionUpdateResponse(EndSession, PlainTextOutput("Come back soon."))
      case Cancel =>
        GameInfoResponse(PlainTextOutput("Cancel?. Just keep playing."))
    }
  }

  def handleGameAction(action: SamsAdventureIntent, session: Session, request: IntentRequest): GameResponse = {
    val gameMap = session.getAttribute("map").asInstanceOf[String].parseJson.convertTo[GameMap]

    //Move or attack
    val result = action match {
      case x : UpdateAction => handleUpdateAction(x, gameMap)
      case x : InfoAction => handleInfoAction(x, gameMap)
    }

    result match {
      case GameUpdateResponse(_, _, newGameMap) =>
        session.setAttribute("map", newGameMap.toJson.compactPrint)
      case _ =>
    }

    result
  }

  def handleUpdateAction(action: UpdateAction, gameMap: GameMap) : GameUpdateResponse = {
    //Move or attack
    val result = action match {
      case Move(direction) => gameMap.move(direction)
      case Attack(direction) => gameMap.attack(direction)
    }

    //Enemies attack
    val updateResults = gameMap.updateEnemies()

    val results = result +: updateResults

    GameUpdateResponse(endState(results), PlainTextOutput(TextEngine.resolveResults(results)), gameMap)
  }

  def handleInfoAction(action: InfoAction, gameMap: GameMap) : GameInfoResponse = {
    val result = action match {
      case Observe(direction) => gameMap.observe(direction)
      case MyHealth => gameMap.playerHealth
      case WhereAmI => gameMap.playerLocation
      case DescribeSurroundings => DescribeSurroundingsResult(gameMap.describeLocation)
    }
    GameInfoResponse(PlainTextOutput(TextEngine.resolveResultText(result)))
  }

  def endState(results : Seq[ActionResult]) : SessionState = {
    results match {
      case Nil => OpenSession
      case (_: Dead) :: xs => EndSession
      case Victory :: xs => EndSession
      case _ :: xs => endState(xs)
    }
  }

  def onLaunch(request: LaunchRequest, session: Session): SpeechletResponse = {
    session.setAttribute("map", baseMap.toJson.compactPrint)

    val welcome = TextEngine.locationMessage(TextEngine.ordinalGroupByTile(baseMap.describeLocation))

    buildResponse(GameInfoResponse(PlainTextOutput(welcome)))
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