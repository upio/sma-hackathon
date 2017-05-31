package com.amazon.hackathon

import com.amazon.hackathon.domain.{Down, Up, Left}
import org.scalatest.FlatSpec

class TextEngineSpec extends FlatSpec{
  "direction text" should "give a single direction string for a direction" in {
    assert(TextEngine.directionText(List(Up)) == "north")
  }

  it should "give multiple direction strings for a multiple directions" in {
    assert(TextEngine.directionText(List(Up, Left, Down)) == "north west and south")
  }

  it should "give use the conjunction for a multiple directions" in {
    assert(TextEngine.directionText(List(Up, Left, Down), conjuction = "blah") == "north west blah south")
  }

  it should "give use the separator for a multiple directions" in {
    assert(TextEngine.directionText(List(Up, Left, Down), separator = ",") == "north,west,and south")
  }


}
