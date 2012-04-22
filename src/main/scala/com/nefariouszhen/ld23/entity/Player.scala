package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.{Game, InputHandler}
import genome.{TallBlob, MiniBlob, Nibble}

class Player(game: Game, val input: InputHandler) extends Mob(game.world) {
  val tile = 60
//  val getShape = new Nibble()
  val tmpPossibleShapes = List(new Nibble, new MiniBlob, new TallBlob)
  var shapeIdx = 0
  def getShape = tmpPossibleShapes(shapeIdx)
  val getColor = 0x00FF00
  var getSpeed = 1

  override def tick() {
    super.tick()

    if (input.tab.down) {
      input.tab.down = false
      shapeIdx = (shapeIdx + 1) % tmpPossibleShapes.size
      getSpeed += 1
    }
  }
}
