package textengine.rules

import com.amazon.hackathon.TextEngine.HitRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class HitRuleSpec extends FlatSpec{

  "HitRule" should "Not modify non hit results" in {
    val actions = List(BumpWall, ObserveResult(Wall, Seq(Up)))
    val results = HitRule.action(actions)

    assert(results == actions)
  }

  it should "not modify a single hit result" in {
    val actions = List(Hit(OrdinalEnemy(Enemy("enemy", 1), Up)))
    val results = HitRule.action(actions)

    assert(results == actions)
  }

  it should "turn multiple hits from one enemy into a single hit" in {
    val enemy = OrdinalEnemy(Enemy("enemy", 1), Up)
    val hit = Hit(enemy)
    val hit2 = Hit(enemy)

    val results = HitRule.action(List(hit, hit2))

    assert(results == List(hit))
  }

  it should "not merge hits with different enemies" in {
    val hit = Hit(OrdinalEnemy(Enemy("enemy1", 1), Up))
    val hit2 = Hit(OrdinalEnemy(Enemy("enemy2", 1), Up))

    val results = HitRule.action(List(hit, hit2))

    assert(results.toSet == Set(hit, hit2))
  }
}
