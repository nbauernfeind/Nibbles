package com.nefariouszhen.ld23.graphics

import com.nefariouszhen.ld23.gen.Point

object Screen {
  val BIT_MIRROR_X = 0x01
  val BIT_MIRROR_Y = 0x02
}

class Screen(val w: Int, val h: Int, sheet: SpriteSheet) {

  import Screen._

  val pixels = Array.ofDim[Int](w * h)
  var offset = Point(0, 0)

  def fill(color: Int) {
    for (i <- 0 until pixels.length) {
      pixels(i) = color
    }
  }

  def render(_xp: Int, _yp: Int, tile: Int, bits: Int) {
    val xp = _xp - offset.x
    val yp = _yp - offset.y

    val mirrorX = (bits & BIT_MIRROR_X) > 0;
    val mirrorY = (bits & BIT_MIRROR_Y) > 0;
    val toffs = 8 * ((tile % sheet.tilesPerRow) + (tile / sheet.tilesPerRow) * sheet.width)

    for (y <- 0 until 8) {
      val ys = if (mirrorY) 7 - y else y
      if (y + yp >= 0 && y + yp < h) {
        for (x <- 0 until 8) {
          if (x + xp >= 0 && x + xp < w) {
            val xs = if (mirrorX) 7 - x else x
            val c = sheet.pixels(xs + ys * sheet.width + toffs);

            // if not transparent color:
            if (c != 0xb4e55e) {
              val idx = x + xp + (y + yp) * w
              if (idx >= pixels.length) {
                println("x %d xp %d y %d yp %d idx %d ttl %d".format(x, xp, y, yp, idx, pixels.length))
              } else {
                pixels((x + xp) + (y + yp) * w) = c;
              }
            }
          }
        }
      }
    }
  }
}
