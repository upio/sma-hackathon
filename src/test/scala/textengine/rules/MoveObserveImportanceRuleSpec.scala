package textengine.rules

import com.amazon.hackathon.TextEngine.MoveObserveImportanceRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class MoveObserveImportanceRuleSpec extends FlatSpec {

  val goal = Moved(OrdinalPlainTile(Goal, Left))
  val enemy = Moved(OrdinalEnemy(Enemy("e", 10), Up))

  "MoveObserveImportanceRule" should "Not modify non moved results" in {
    val actions = List(BumpWall, ObserveResult(Wall, Seq(Up)))
    val results = MoveObserveImportanceRule.action(actions)

    assert(results == actions)
  }

  it should "not remove important plain tiles" in  {
    val results = MoveObserveImportanceRule.action(List(goal))

    assert(results == List(goal))
  }

  it should "not remove enemy tiles" in  {
    val results = MoveObserveImportanceRule.action(List(enemy))

    assert(results == List(enemy))
  }

  it should "remove unimportant tiles" in  {
    val results = MoveObserveImportanceRule.action(List(Moved(OrdinalPlainTile(Wall, Up))))

    assert(results == List())
  }

  it should "resolve a collection of actions" in  {
    val results = MoveObserveImportanceRule.action(List(enemy, goal, Moved(OrdinalPlainTile(Wall, Up)), BumpWall))

    assert(results.toSet == Set(enemy, goal, BumpWall))
  }
}
