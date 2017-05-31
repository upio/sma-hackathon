package textengine.rules

import com.amazon.hackathon.TextEngine.HurtRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class HurtRuleSpec extends FlatSpec{

  "HurtRule" should "Not modify non hurt results" in {
    val actions = List(BumpWall, ObserveResult(Wall, Seq(Up)))
    val results = HurtRule.action(actions)

    assert(results == actions)
  }

  it should "not modify a single hurt result" in {
    val actions = List(Hurt(OrdinalEnemy(Enemy("enemy", 1), Up), 1))
    val results = HurtRule.action(actions)

    assert(results == actions)
  }

  it should "turn multiple hurts from one enemy into a single multihurt with the minimum health remaining" in {
    val enemy = OrdinalEnemy(Enemy("enemy", 1), Up)
    val hurt1 = Hurt(enemy, 1)
    val hurt2 = Hurt(enemy, 2)

    val results = HurtRule.action(List(hurt1, hurt2))

    assert(results == List(MultiHurt(enemy, 1, 2)))
  }

  it should "not merge hurts with different enemies" in {
    val hurt1 = Hurt(OrdinalEnemy(Enemy("enemy1", 1), Up), 1)
    val hurt2 = Hurt(OrdinalEnemy(Enemy("enemy2", 1), Up), 2)

    val results = HurtRule.action(List(hurt1, hurt2))

    assert(results.toSet == Set(hurt1, hurt2))
  }
}
