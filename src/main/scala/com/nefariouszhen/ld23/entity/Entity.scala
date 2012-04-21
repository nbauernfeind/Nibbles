package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.gen.{World, Point}

abstract class Entity(world: World) {
  var (x, y) = (0, 0)
  var (xr, yr) = (6, 6)
  var removed = false

  def render(screen: Screen)
  def tick() {}

  def getPos = Point(x >> 4, y >> 4)

  def remove() {
    removed = true
  }

  def move(xa: Int, ya: Int): Boolean = {
    var stopped = false
    if (xa != 0 || ya != 0) {
      stopped = true

      if (xa != 0 && move2(xa, 0)) stopped = false
      if (ya != 0 && move2(0, ya)) stopped = false
      if (!stopped) {
        world.getTile(getPos).steppedOn(world, getPos, this)
      }
    }

    !stopped
  }

  private[this] def move2(xa: Int, ya: Int): Boolean = {
    false
  }
}
