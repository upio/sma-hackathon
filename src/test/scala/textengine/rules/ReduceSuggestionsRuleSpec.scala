package textengine.rules

import com.amazon.hackathon.TextEngine.ReduceSuggestionsRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class ReduceSuggestionsRuleSpec extends FlatSpec {

  "ReduceSuggestionsRule" should "Not modify non suggestion results" in {
    val actions = List(BumpWall, ObserveResult(Wall, Seq(Up)))
    val results = ReduceSuggestionsRule.action(actions)

    assert(results == actions)
  }

  it should "not modify a single attack suggestion" in {
    val results = ReduceSuggestionsRule.action(List(AttackSuggestion(Up)))

    assert(results == List(AttackSuggestion(Up)))
  }

  it should "not modify a single move suggestion" in {
    val results = ReduceSuggestionsRule.action(List(MoveSuggestion(Up)))

    assert(results == List(MoveSuggestion(Up)))
  }

  it should "reduce multiple attack suggestions" in {
    val results = ReduceSuggestionsRule.action(List(AttackSuggestion(Up), AttackSuggestion(Down)))

    assert(results == List(MultiAttackSuggestion(List(Up, Down))))
  }

  it should "reduce multiple move suggestions" in {
    val results = ReduceSuggestionsRule.action(List(MoveSuggestion(Up), MoveSuggestion(Down)))

    assert(results == List(MultiMoveSuggestion(List(Up, Down))))
  }
}
