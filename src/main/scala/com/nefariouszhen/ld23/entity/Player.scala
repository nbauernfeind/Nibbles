package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.{Game, InputHandler}
import com.nefariouszhen.ld23.graphics.Screen

class Player(game: Game, input: InputHandler) extends Mob(game.world) {
  val tile = 60

  xr = 3
  yr = 3

  def render(screen: Screen) {
    screen.render(x - 4, y - 4, ((walkDist >> 4) & 1) + 3 * 20, 0)
  }

  override def tick() {
    var (dx, dy) = (0, 0)
    if (input.up.down) dy -= 1
    if (input.down.down) dy += 1
    if (input.left.down) dx -= 1
    if (input.right.down) dx += 1
    move(dx, dy)
  }
}
