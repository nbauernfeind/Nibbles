package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.{Game, InputHandler}
import com.nefariouszhen.ld23.graphics.Screen

class Player(game: Game, input: InputHandler) extends Mob(game.world) {
  val tile = 60
  def render(screen: Screen) {
    screen.render(x-4, y-4, 0 + 3*20, 0)
  }
}
