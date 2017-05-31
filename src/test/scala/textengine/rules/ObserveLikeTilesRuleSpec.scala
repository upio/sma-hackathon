package textengine.rules

import com.amazon.hackathon.TextEngine.ObserveLikeTilesRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class ObserveLikeTilesRuleSpec extends FlatSpec {
  "ObserveLikeTilesRule" should "not chance non moved or describe results" in {
    val actions = List(BumpWall, AttackSuggestion(Up))
    val results = ObserveLikeTilesRule.action(actions)

    assert(results == actions)
  }

  it should "reduce like plain tile types" in {
    val actions = List(Moved(OrdinalPlainTile(Blank, Up)), Moved(OrdinalPlainTile(Blank, Down)))
    val results = ObserveLikeTilesRule.action(actions)

    assert(results == List(ObserveResult(Blank, List(Up, Down))))
  }

  it should "reduce like plain tile types for single result" in {
    val actions = List(Moved(OrdinalPlainTile(Blank, Up)))
    val results = ObserveLikeTilesRule.action(actions)

    assert(results == List(ObserveResult(Blank, List(Up))))
  }

  it should "doesn't reduce enemy tiles" in {
    val enemy1 = Enemy("e1", 1);
    val enemy2 = Enemy("e2", 1)
    val actions = List(Moved(OrdinalEnemy(enemy1, Up)), Moved(OrdinalEnemy(enemy2, Down)))
    val results = ObserveLikeTilesRule.action(actions)

    assert(results == List(ObserveResult(enemy1, List(Up)), ObserveResult(enemy2, List(Down))))
  }

  it should "reduce like DescribeSurroundingsResult plain tile types" in {
    val actions = List(DescribeSurroundingsResult(OrdinalPlainTile(Blank, Up)), DescribeSurroundingsResult(OrdinalPlainTile(Blank, Down)))
    val results = ObserveLikeTilesRule.action(actions)

    assert(results == List(ObserveResult(Blank, List(Up, Down))))
  }

}
