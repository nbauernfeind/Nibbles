package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.gen.{World, Point}

abstract class Entity(world: World) {
  var (x, y) = (0, 0)
  var removed = false

  def xr: Int
  def yr: Int

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

  def intersects(x0: Int, y0: Int, x1: Int, y1: Int): Boolean = {
    !(x + xr < x0 || y + yr < y0 || x - xr > x1 || y - yr > y1)
  }

  private[this] def move2(xa: Int, ya: Int): Boolean = {
    val p0 = Point.toPoint(x - xr, y - yr)
    val p1 = Point.toPoint(x + xr - 1, y + yr - 1)

    val t0 = Point.toPoint(x + xa - xr, y + ya - yr)
    val t1 = Point.toPoint(x + xa + xr - 1, y + ya + yr - 1)

    for (y <- t0.y to t1.y; x <- t0.x to t1.x) {
      if (!(x >= p0.x && x <= p1.x && y >= p0.y && y <= p1.y)) {
        val p = Point(x,y)
        val tile = world.getTile(p)
        //world.getTile(p).bumpedInto(level, p, this)

        if (!tile.mayPass(world, p, this)) {
          return false
        }
      }
    }

    x += xa
    y += ya
    true
  }
}
