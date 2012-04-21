package com.nefariouszhen.ld23.graphics

import java.awt.image.BufferedImage

case class Sprite(x: Int, y: Int, img: Int, bits: Int)

class SpriteSheet(image: BufferedImage) {
  val width = image.getWidth
  val height = image.getHeight
  val tilesPerRow = width / 8
  val pixels = image.getRGB(0, 0, width, height, null, 0, width)
}