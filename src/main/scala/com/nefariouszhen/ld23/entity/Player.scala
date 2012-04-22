package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.{Game, InputHandler}
import genome._

class Player(game: Game, val input: InputHandler) extends Mob(game.world) {
  var lvl = 1
  var bank = new MemoryBank(4, List(SpeedBoost(4), SightBoost(2), HealthBoost(1)))
}
