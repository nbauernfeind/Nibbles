package com.nefariouszhen.ld23.entity

import genome.MiniBlob
import com.nefariouszhen.ld23.gen.World

class Enemy(world: World) extends Mob(world) {
  val getShape = new MiniBlob
  val getColor = rand.nextInt()
  val getSpeed = rand.nextInt(2) + 1
}
