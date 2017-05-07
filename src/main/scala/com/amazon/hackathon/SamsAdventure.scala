/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon


import com.amazon.hackathon.domain._
import com.amazon.speech.slu.Slot
import com.amazon.speech.speechlet._

import scala.collection.JavaConverters._
import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler
import com.amazon.speech.ui.{OutputSpeech, PlainTextOutputSpeech, Reprompt, SsmlOutputSpeech}
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

  private def tileMessage(tile: Tile): String = tile match {
    case Blank => "empty space"
    case Wall => s"a wall"
    case Goal => "the stairs down"
    case Enemy(name, health) => s"a $name with $health health"
    case _ => "oopsy-daisy"
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

    action match {
      case x: SystemAction => handleSystemAction(x)
      case x: SamsAdventureIntent => handleGameAction(x, session, request)
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

  def handleSystemAction(action: SystemAction): SpeechletResponse = {
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

    //Move or attack
    val result = action match {
      case x : UpdateAction => handleUpdateAction(x, gameMap)
      case x : InfoAction => handleInfoAction(x, gameMap)
    }

    val response = new SpeechletResponse()

    val speech = result match {
      case GameUpdateResponse(sessionState, gameSpeechOutput, newGameMap) =>
        session.setAttribute("map", newGameMap.toJson.compactPrint)
        response.setShouldEndSession(sessionState match {
          case EndSession => true
          case OpenSession => false
        })
        gameSpeechOutput
      case GameInfoResponse(gameSpeechOutput) =>
        response.setShouldEndSession(false)
        gameSpeechOutput
    }

    val speechObj = speech match {
      case PlainTextOutput(message) =>
        val s = new PlainTextOutputSpeech()
        s.setText(message)
        s
      case SSMLOutput(message) =>
        val s = new SsmlOutputSpeech()
        s.setSsml(message)
        s
    }

    response.setOutputSpeech(speechObj)

    response
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

    GameUpdateResponse(endState(results), PlainTextOutput(resolveResults(results)), gameMap)
  }

  def handleInfoAction(action: InfoAction, gameMap: GameMap) : GameInfoResponse = {
    val result = action match {
      case Observe(direction) => gameMap.observe(direction)
      case MyHealth => gameMap.playerHealth
      case WhereAmI => gameMap.playerLocation
      case DescribeSurroundings => DescribeSurroundingsResult(gameMap.describeLocation)
    }
    GameInfoResponse(PlainTextOutput(resolveResultText(result)))
  }

  def endState(results : Seq[ActionResult]) : SessionState = {
    results match {
      case Nil => OpenSession
      case (_: Dead) :: xs => EndSession
      case Victory :: xs => EndSession
      case _ :: xs => endState(xs)
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

  def resolveResultText(result : ActionResult) : String = {
    result match {
      case NothingThere => "There is nothing to attack"

      case Killed(Enemy(name, _)) => s"You killed a $name"

      case Hit(Enemy(name, health)) => s"You hit the $name, it now has $health health"

      case BumpWall => "You walked into a wall dummy."

      case Moved(directions) => locationMessage(directions, shortResult = true)

      case Hurt(Enemy(name, enemyHealth), health) => s"you were hurt by a $name. You now have $health health"

      case Dead(Enemy(name, eh)) => s"you are dead, Killed by a $name. It stares down at your lifeless body, laughing."

      case Victory => "You found the stairs down to the next dungeon. Unfortunately the ghouls are still building it. Come back soon."

      case MyHealthResult(health) => s"You have $health"

      case WhereAmIResult(x,y) => s"You are at position $x, $y"

      case ObserveResult(tile) => tileMessage(tile)

      case DescribeSurroundingsResult(directions) => locationMessage(directions, shortResult = false)
    }
  }

  def ignoreTileOnShortResult (tile: Tile) : Boolean = {
    tile match {
      case Blank => true
      case _ => false
    }
  }

  def locationMessage(directions:  Ordinal, shortResult: Boolean): String = {
    val map = directions match {
      case Ordinal(left,right,up,down) => Map ("west" -> left, "east" -> right, "north" -> up, "south" -> down)
    }

    val a: Seq[(Tile, Seq[String])] = map.toSeq.groupBy(_._2)
      .filter{ x => !shortResult || !ignoreTileOnShortResult(x._1)}
      .map { case (tile, group) => tile -> group.map(_._1) }.toSeq

    a.map { x => tileMessage(x._1) + " is to the " + directionText(x._2.toList) }.mkString(". ")
  }

  def directionText(directions: List[String]): String = {
    directions match {
      case Nil => ""
      case x :: Nil => x
      case x :: y :: Nil => x + " and " + y
      case x :: xs => x + " " + directionText(xs)
    }
  }

  def onLaunch(request: LaunchRequest, session: Session): SpeechletResponse = {
    session.setAttribute("map", baseMap.toJson.compactPrint)

    val welcome = locationMessage(baseMap.describeLocation, shortResult = false)

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