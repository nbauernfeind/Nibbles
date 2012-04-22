package com.nefariouszhen.ld23.entity.particle

import com.nefariouszhen.ld23.entity.Entity
import com.nefariouszhen.ld23.graphics.{Font, Screen}
import com.nefariouszhen.ld23.gen.World

class TextParticle(world: World, msg: String, _x: Int, _y: Int, col: Int) extends Entity(world) {
  x = _x
  y = _y

  var (xx, yy, zz) = (x.toDouble, y.toDouble, 2.toDouble)
  var (xa, ya, za) = (rand.nextGaussian() * .3, rand.nextGaussian() * .2, rand.nextDouble() * .7 + 2)
  var time = 0

  override def tick() {
    time += 1
    if (time > 30) remove()

    xx += xa
    yy += ya
    zz += za

    if (zz < 0) {
      zz = 0
      xa *= .6
      ya *= .6
      za *= -.5
    }

    za -= .15

    x = xx.toInt
    y = yy.toInt
  }

  def xr = 0
  def yr = 0

  def render(screen: Screen) {
    Font.draw(msg, screen, x - msg.length * 4 + 1, y - zz.toInt + 1, 0)
    Font.draw(msg, screen, x - msg.length * 4, y - zz.toInt, col)
  }
}
