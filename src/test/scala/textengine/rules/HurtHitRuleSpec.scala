package textengine.rules

import com.amazon.hackathon.TextEngine.HurtHitRule
import com.amazon.hackathon.domain._
import org.scalatest.FlatSpec

class HurtHitRuleSpec extends FlatSpec {

  val enemy1 = OrdinalEnemy(Enemy("enemy", 10), Up)
  val hurt = Hurt(enemy1, 1)
  val hit = Hit(enemy1)
  val multiHurt = MultiHurt(enemy1, 10, 1)


  "HurtHitRule" should "Not modify non hit, hurt, or multihurt results" in {
    val actions = List(BumpWall, ObserveResult(Wall, Seq(Up)))
    val results = HurtHitRule.action(actions)

    assert(results == actions)
  }

  it should "merge a hurt and hit from the same enemy" in {
    val result = HurtHitRule.action(List(hurt, hit))

    assert(result.head == HurtHit(hurt, hit))
  }

  it should "merge a hit and hurt from the same enemy" in {
    val result = HurtHitRule.action(List(hit, hurt))

    assert(result.head == HurtHit(hurt, hit))
  }

  it should "merge a multihurt and hit from the same enemy" in {
    val result = HurtHitRule.action(List(multiHurt, hit))

    assert(result.head == MultiHurtHit(multiHurt, hit))
  }

  it should "merge a hit and multihurt from the same enemy" in {
    val result = HurtHitRule.action(List(hit, multiHurt))

    assert(result.head == MultiHurtHit(multiHurt, hit))
  }

  it should "ignore a lone hurt" in {
    val result = HurtHitRule.action(List(hurt))

    assert(result.head == hurt)
  }

  it should "ignore a lone hit" in {
    val result = HurtHitRule.action(List(hit))

    assert(result.head == hit)
  }

  it should "ignore a lone multihurt" in {
    val result = HurtHitRule.action(List(multiHurt))

    assert(result.head == multiHurt)
  }

  it should "ignore a hurt and hit of different enemies" in {
    val otherHit = Hit(OrdinalEnemy(Enemy("e2", 10), Up))
    val result = HurtHitRule.action(List(hurt, otherHit))

    assert(result.toSet == Set(hurt, otherHit))
  }
}
