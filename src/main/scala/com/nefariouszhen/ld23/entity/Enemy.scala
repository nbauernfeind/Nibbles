package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.gen.World
import genome.{TallBlob, MiniBlob}

class Enemy(world: World) extends Mob(world) {
  val getShape = if (rand.nextBoolean()) new MiniBlob else new TallBlob
  val getColor = rand.nextInt()
  val getSpeed = rand.nextInt(2) + 1
}
