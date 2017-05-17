/**
  * Copyright 2017 Amazon, Inc. or its affiliates. All Rights Reserved.
  */
package com.amazon.hackathon


import com.amazon.hackathon.domain.{BeScared, _}
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

  def onIntent(request: IntentRequest, session: Session): SpeechletResponse = {
    println(request.getIntent.getName)
    println(request.getIntent.getSlots.asScala)

    val promptWait = getPromptWaitState(session)

    val intent = request.getIntent

    val action = intent.getName match {
      case "AMAZON.StopIntent" => Stop
      case "AMAZON.HelpIntent" => Help
      case "AMAZON.CancelIntent" => Cancel
        //if currently in a prompt, don't accept any external intents
      case i if !Option(promptWait).getOrElse("").isEmpty =>
        i match {
          case "confirmPromptIntent" => PromptAction(promptWait, getPromptConfirmationFromSlot(intent.getSlots.asScala.toMap))
          case _ => PromptAction(promptWait, Unknown)
        }
      case "moveIntent" => Move(getDirectionFromSlot(intent.getSlots.asScala.toMap))
      case "attackIntent" => Attack(getDirectionFromSlot(intent.getSlots.asScala.toMap))
      case "observeIntent" => Observe(getDirectionFromSlot(intent.getSlots.asScala.toMap))
      case "whereAmIIntent" => WhereAmI
      case "describeSurroundingsIntent" => DescribeSurroundings
      case "myHealthIntent" => MyHealth
      case "scaredIntent" => setPromptWaitState(session, "SCARED_HUG")
                              BeScared
      case _ => UnknownAction
    }

    buildResponse(action match {
      case x: SystemAction => handleSystemAction(x)
      case x: SamsAdventureIntent => handleGameAction(x, session, request)
      case PromptAction("START_READY", response) => handlePromptResponse(session,handleStartReadyPrompt(response))
      case PromptAction("SCARED_HUG", response) => handlePromptResponse(session,handleScaredHugPrompt(response))
      case UnknownAction | _ => GameInfoResponse(PlainTextOutput("Huh, please repeat that. You know you can always ask me for help."))
    })
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
        new SsmlOutputSpeech(){ setSsml("<speak>" + message + "</speak>") }
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

  def getPromptConfirmationFromSlot(slots: Map[String, Slot]): PromptResponse = {
    println(slots)
    slots.values.find(_.getValue != null).get.getName match {
      case "yesSlot" => Yes
      case "noSlot" => No
      case _ => Unknown
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

  def handlePromptResponse(session: Session, promptUpdateResponse: PromptUpdateResponse) : SessionUpdateResponse = {
    promptUpdateResponse match {
        //finish current prompt
      case PromptFinishedResponse(sessionState, gameSpeechOutput) =>
        clearPromptWaitState(session)
        SessionUpdateResponse(sessionState, gameSpeechOutput)
        //maintain or chain prompts
      case PromptOpenResponse(sessionState, gameSpeechOutput, waitPromptState) =>
        setPromptWaitState(session, waitPromptState)
        SessionUpdateResponse(sessionState, gameSpeechOutput)
    }
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
      case BeScared => IsScared
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

  def handleStartReadyPrompt(promptResponse: PromptResponse) : PromptUpdateResponse = {
    promptResponse match {
      case Yes => PromptFinishedResponse(OpenSession,
        SSMLOutput("<emphasis level=\"reduced\"><prosody volume=\"x-loud\" rate=\"fast\">Fine</prosody></emphasis>,<break strength=\"x-strong\"/>" +
          "but I am coming with you. <break strength=\"x-strong\"/><prosody volume=\"x-soft\">The treasure deep in the dungeon will make up for it anyways.</prosody><break strength=\"x-strong\"/>" +
          "Head down those stairs and lets get this over with. <break strength=\"x-strong\"/> I told you it was going to be dark.<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>" +
          "ask me about your surroundings if you want know what my sensors \"see\"?"))
      case No => PromptFinishedResponse(EndSession, PlainTextOutput("Well, goodbye I guess"))
      case Unknown => PromptOpenResponse(OpenSession, PlainTextOutput("Huh? Speak up. I can't understand you. Please repeat."), "START_READY")
    }
  }

  def handleScaredHugPrompt(promptResponse: PromptResponse) : PromptUpdateResponse = {
    promptResponse match {
      case Yes => PromptFinishedResponse(OpenSession,
        PlainTextOutput("Ohh suck it up, this was your idea in the first place"))
      case No => PromptFinishedResponse(OpenSession, PlainTextOutput("Well, ok then"))
      case Unknown => PromptFinishedResponse(OpenSession, PlainTextOutput("Huh? Anyways, shall we continue?"))
    }
  }

  def onLaunch(request: LaunchRequest, session: Session): SpeechletResponse = {
    session.setAttribute("map", baseMap.toJson.compactPrint)
    setPromptWaitState(session, "START_READY")

    val welcome =
      SSMLOutput("<emphasis level=\"reduced\"><prosody volume=\"x-loud\">wait!!</prosody></emphasis><break strength=\"x-strong\"/>Slow down <emphasis level=\"reduced\"><prosody volume=\"x-loud\">Hero.</prosody></emphasis><break strength=\"x-strong\"/>You won't be able to see anything in there. <emphasis level=\"reduced\"><prosody volume=\"loud\">This is your last chance to turn around</prosody></emphasis><break strength=\"x-strong\"/><break strength=\"x-strong\"/>Are you sure you want to enter the dungeon?") //TextEngine.locationMessage(TextEngine.ordinalGroupByTile(baseMap.describeLocation))

    buildResponse(GameInfoResponse(welcome))
  }

  def setPromptWaitState(session: Session, state : String) : Unit =
    session.setAttribute("promptWaitState", state)

  def clearPromptWaitState(session : Session) : Unit =
    session.removeAttribute("promptWaitState")

  def getPromptWaitState(session: Session) : String =
    session.getAttribute("promptWaitState").asInstanceOf[String]

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