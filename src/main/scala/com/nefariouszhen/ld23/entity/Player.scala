package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.{Game, InputHandler}
import genome.{MiniBlob, Nibble}

class Player(game: Game, val input: InputHandler) extends Mob(game.world) {
  val tile = 60
//  val getShape = new Nibble()
  val getShape = new MiniBlob()
  val getColor = 0x00FF00
  val getSpeed = 1

//  override def tick() {
//    super.tick()
//
//    var (dx, dy) = (0, 0)
//    if (input.up.down) dy -= 1
//    if (input.down.down) dy += 1
//    if (input.left.down) dx -= 1
//    if (input.right.down) dx += 1
//    move(dx, dy)
//  }
}
