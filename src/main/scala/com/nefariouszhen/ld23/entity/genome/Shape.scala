package com.nefariouszhen.ld23.entity.genome

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.entity.Mob

trait Shape {
  def xr: Int
  def yr: Int
  def render(screen: Screen, mob: Mob)
}

class TwoStepShape(val xr: Int, val yr: Int, spriteOffset: Int, speedShift: Int) extends Shape {
  def render(screen: Screen, mob: Mob) {
    screen.render(mob.x - 4, mob.y - 4, ((mob.walkDist >> speedShift) & 1) + spriteOffset, 0, mob.getColor)
  }
}

class Nibble extends TwoStepShape(3, 3, 0 + 3 * 20, 4)
class MiniBlob extends TwoStepShape(3, 2, 2 + 3 * 20, 4)
