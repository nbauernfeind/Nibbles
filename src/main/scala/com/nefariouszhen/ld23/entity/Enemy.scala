package com.nefariouszhen.ld23.entity

import genome.MiniBlob
import com.nefariouszhen.ld23.gen.Direction._
import com.nefariouszhen.ld23.gen.{World, Direction}

class Enemy(world: World) extends Mob(world) {
  val getShape = new MiniBlob
  val getColor = rand.nextInt()

  override def tick() {
    super.tick()

    if (rand.nextInt(100) < 20) {
      Direction.nextDir(rand) match {
        case NORTH => move(0, -1)
        case SOUTH => move(0, 1)
        case EAST => move(-1, 0)
        case WEST => move(1, 0)
      }
    }
  }
}
