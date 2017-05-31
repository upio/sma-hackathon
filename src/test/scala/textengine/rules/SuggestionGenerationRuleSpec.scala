package textengine.rules

import com.amazon.hackathon.TextEngine.SuggestionGeneration
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

/**
  * Created by sussmans on 5/30/2017.
  */
class SuggestionGenerationRuleSpec extends FlatSpec {
  "Suggestion Generation" should "generate move suggestion for moved result to move tiles" in {
    val suggestions = SuggestionGeneration.action(List(Moved(OrdinalPlainTile(Blank, Up))))

    assert(suggestions.head == MoveSuggestion(Up))
  }

  it should "generate multiple move suggestions for multiple move results to multi move tiles" in {
    val suggestions = SuggestionGeneration.action(List(Moved(OrdinalPlainTile(Blank, Up)), Moved(OrdinalPlainTile(Blank, Down))))

    assert(suggestions.head == MoveSuggestion(Up))
    assert(suggestions.tail.head == MoveSuggestion(Down))
  }

  it should "maintain original elements" in {
    val actions = List(Moved(OrdinalPlainTile(Blank, Up)), Victory, BumpWall)
    val suggestions = SuggestionGeneration.action(actions)

    assert(suggestions.tail == actions)
  }

  it should "ignore non-move tile moved results" in {
    val actions = List(Moved(OrdinalPlainTile(Wall, Up)))
    val suggestions = SuggestionGeneration.action(actions)

    assert(suggestions == actions)
  }

  it should "generate attack suggestion for enemy result" in {
    val enemy = Enemy("test", 1)
    val suggestions = SuggestionGeneration.action(List(Moved(OrdinalEnemy(enemy, Up))))

    assert(suggestions.head == AttackSuggestion(Up))
  }

  it should "generate attack suggestions for multiple enemy result" in {
    val enemy = Enemy("test", 1)
    val suggestions = SuggestionGeneration.action(List(Moved(OrdinalEnemy(enemy, Up)), Moved(OrdinalEnemy(enemy, Down))))

    assert(suggestions.head == AttackSuggestion(Up))
    assert(suggestions.tail.head == AttackSuggestion(Down))
  }

  it should "generate attack and move suggestions for move and enemy tiles" in {
    val enemy = Enemy("test", 1)
    val suggestions = SuggestionGeneration.action(List(Moved(OrdinalPlainTile(Blank, Up)), Moved(OrdinalEnemy(enemy, Down))))

    assert(suggestions.head == MoveSuggestion(Up))
    assert(suggestions.tail.head == AttackSuggestion(Down))
  }
}
