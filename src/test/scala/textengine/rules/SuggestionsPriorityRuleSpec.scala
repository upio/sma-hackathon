package textengine.rules

import com.amazon.hackathon.TextEngine.SuggestionsPriorityRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class SuggestionsPriorityRuleSpec extends FlatSpec {

  "Suggestion Priority Rule" should "modify non suggestion results" in {
    val actions = List(BumpWall, ObserveResult(Wall, Seq(Up)))
    val results = SuggestionsPriorityRule.action(actions)

    assert(results == actions)
  }

  it should "leave move suggestions when no attack suggestions are present" in {
    val results = SuggestionsPriorityRule.action(List(MoveSuggestion(Up)))

    assert(results == List(MoveSuggestion(Up)))
  }

  it should "not change any results when only attack suggesitons are present" in {
    val results = SuggestionsPriorityRule.action(List(AttackSuggestion(Up)))

    assert(results == List(AttackSuggestion(Up)))
  }

  it should "remove move suggesions when attack suggestions are present" in {
    val results = SuggestionsPriorityRule.action(List(AttackSuggestion(Up), MoveSuggestion(Up)))

    assert(results == List(AttackSuggestion(Up)))
  }

  it should "remove move suggesions when attack suggestions are present regardless of the order" in {
    val results = SuggestionsPriorityRule.action(List(MoveSuggestion(Up), AttackSuggestion(Up)))

    assert(results == List(AttackSuggestion(Up)))
  }

  it should "not modify non suggestion results" in {
    val results = SuggestionsPriorityRule.action(List(MoveSuggestion(Up), AttackSuggestion(Up), BumpWall))

    assert(results.toSet == Set(AttackSuggestion(Up), BumpWall))
  }


}
