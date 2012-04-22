package com.nefariouszhen.ld23.entity.genome

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.entity.{Enemy, Player, Mob}

case class SpriteInfo(ox: Int, oy: Int, w: Int, h: Int) {
  def offset = ox + oy * 20
}

trait Shape {
  var (xa, ya) = (0, 0)

  def si: SpriteInfo
  def xr: Int
  def yr: Int
  def render(screen: Screen, mob: Mob)

  def enemyTick(mob: Mob)

  def playerTick(p: Player) {
    xa = 0
    ya = 0

    if (p.input.up.down) ya -= 1
    if (p.input.down.down) ya += 1
    if (p.input.left.down) xa -= 1
    if (p.input.right.down) xa += 1
  }

  def tick(mob: Mob) {
    mob match {
      case e: Enemy => enemyTick(mob)
      case p: Player => playerTick(p)
    }

    var s = math.min(15, mob.getSpeed)
    var m = 1
    while (s > 0) {
      val r = 8 / m
      val i = s / r

      if (i > 0 && mob.tickTime % m == 0)
        mob.move(xa, ya)

      s %= r
      m += 1
    }
  }

  def renderFrame(screen: Screen, mob: Mob, t: Int) {
    screen.renderSprite(mob.x, mob.y, si, mob.getColor, t)
  }
}

class TwoStepShape(val xr: Int, val yr: Int, val si: SpriteInfo, speedShift: Int) extends Shape {
  def render(screen: Screen, mob: Mob) {
    val t = ((mob.walkDist) >> speedShift) & 1
    renderFrame(screen, mob, t)
  }

  def enemyTick(mob: Mob) {}
}

class JumpingShape(val xr: Int, val yr: Int, val si: SpriteInfo, jumpInterval: Int, restInterval: Int) extends Shape {
  var jumpTime = 0

  def enemyTick(mob: Mob) {
    if (mob.rand.nextInt(40) == 0 && jumpTime <= -restInterval) {
      xa = mob.rand.nextInt(3) - 1
      ya = mob.rand.nextInt(3) - 1

      val (dx, dy) = mob.distToPlayer
      if ((dx * dx + dy * dy) < mob.sightR2) {
        if (dx < 0) xa = -1
        if (dx > 0) xa = +1
        if (dy < 0) ya = -1
        if (dy > 0) ya = +1
      }
    }
  }

  override def tick(mob: Mob) {
    jumpTime -= (math.min(15,mob.getSpeed)) / 8 + 1
    if (jumpTime <= 0) {
      xa = 0
      ya = 0
    }

    super.tick(mob)

    if (jumpTime < -restInterval && (xa != 0 || ya != 0))
      jumpTime = jumpInterval
  }

  def render(screen: Screen, mob: Mob) {
    val t = if (jumpTime > 0) 1 else 0
    renderFrame(screen, mob, t)
  }
}

class Nibble extends TwoStepShape(3, 3, SpriteInfo(0, 3, 1, 1), 4)
class MiniBlob extends JumpingShape(3, 2, SpriteInfo(2, 3, 1, 1), 10, 5)
class TallBlob extends JumpingShape(3, 3, SpriteInfo(4, 3, 1, 2), 15, 10)
class GiganticBlob extends JumpingShape(7, 6, SpriteInfo(6, 3, 2, 2), 20, 15)

