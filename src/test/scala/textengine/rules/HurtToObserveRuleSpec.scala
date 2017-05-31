package textengine.rules

import com.amazon.hackathon.TextEngine.HurtToObserveRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class HurtToObserveRuleSpec extends FlatSpec{
  "HurtToObserveRule" should "not modify non Moved results" in {
    val actions = List(Hurt(OrdinalEnemy(Enemy("e", 1), Up), 1), BumpWall)
    val results = HurtToObserveRule.action(actions)

    assert(results.toSet == actions.toSet)
  }

  it should "remove moved results with matching hurts" in {
    val enemy = OrdinalEnemy(Enemy("e1", 1), Up)
    val actions = List(Hurt(enemy, 1), Moved(enemy))
    val results = HurtToObserveRule.action(actions)

    assert(results.toSet == Set(Hurt(enemy, 1)))
  }

  it should "remove moved results with matching hurts hits" in {
    val enemy = OrdinalEnemy(Enemy("e1", 1), Up)
    val actions = List(HurtHit(Hurt(enemy, 1), Hit(enemy)), Moved(enemy))
    val results = HurtToObserveRule.action(actions)

    assert(results.toSet == Set(actions.head))
  }

  it should "remove moved results with matching multi hurts hits" in {
    val enemy = OrdinalEnemy(Enemy("e1", 1), Up)
    val actions = List(MultiHurtHit(MultiHurt(enemy, 1, 1), Hit(enemy)), Moved(enemy))
    val results = HurtToObserveRule.action(actions)

    assert(results.toSet == Set(actions.head))
  }

  it should "remove moved results with matching multi hurts" in {
    val enemy = OrdinalEnemy(Enemy("e1", 1), Up)
    val actions = List(MultiHurt(enemy, 1, 1), Moved(enemy))
    val results = HurtToObserveRule.action(actions)

    assert(results.toSet == Set(actions.head))
  }

  it should "not remove moved results without matching hurts" in {
    val enemy = OrdinalEnemy(Enemy("e1", 1), Up)
    val enemy2 = OrdinalEnemy(Enemy("e2", 1), Up)
    val actions = List(Hurt(enemy, 1), Moved(enemy2))
    val results = HurtToObserveRule.action(actions)

    assert(results.toSet == Set(Hurt(enemy, 1), Moved(enemy2)))
  }


}
