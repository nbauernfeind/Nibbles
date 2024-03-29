package com.nefariouszhen.ld23.entity.particle

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.gen.{Point, World, Direction}
import com.nefariouszhen.ld23.entity.{Mob, Entity}
import com.nefariouszhen.ld23.gen.Direction._

class AttackParticle(mob: Mob, world: World, _r: Int, d: Direction) extends Entity(world) {
  x = mob.x - mob.xr
  y = mob.y

  if (d == NORTH) y -= 2 * mob.yr

  var time = 0
  var r = _r

  override def tick() {
    super.tick()

    time += 1

    if (r <= 0) {
      remove()
    } else {
      val nd = r / math.max(1,(10 - time))
      for (i <- 0 until nd) {
        val np = Point(x, y).move(d)
        r -= 1
        x = np.x
        y = np.y
      }
    }
  }

  def render(screen: Screen) {
    screen.render(x, y, 1 + 11 * 20, 0, mob.getColor)
  }
}
