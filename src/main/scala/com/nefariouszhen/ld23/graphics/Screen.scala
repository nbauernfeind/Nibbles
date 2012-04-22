package com.nefariouszhen.ld23.graphics


object Screen {
  val BIT_MIRROR_X = 0x01
  val BIT_MIRROR_Y = 0x02
}

class Screen(val w: Int, val h: Int, sheet: SpriteSheet) {

  import Screen._

  val pixels = Array.ofDim[Int](w * h)
  var offset = Tuple2(0, 0)

  def fill(color: Int) {
    for (i <- 0 until pixels.length) {
      pixels(i) = color
    }
  }

  def render(_xp: Int, _yp: Int, tile: Int, bits: Int, color: Int = 0xffffff) {
    val xp = _xp - offset._1
    val yp = _yp - offset._2

    val mirrorX = (bits & BIT_MIRROR_X) > 0;
    val mirrorY = (bits & BIT_MIRROR_Y) > 0;
    val toffs = 8 * ((tile % sheet.tilesPerRow) + (tile / sheet.tilesPerRow) * sheet.width)

    for (y <- 0 until 8) {
      val ys = if (mirrorY) 7 - y else y
      if (y + yp >= 0 && y + yp < h) {
        for (x <- 0 until 8) {
          if (x + xp >= 0 && x + xp < w) {
            val xs = if (mirrorX) 7 - x else x
            val c = sheet.pixels(xs + ys * sheet.width + toffs)

            // Heh, transparent booger green
            if ((c & 0xffffff) != 0xb4e55e) {
              val idx = x + xp + (y + yp) * w
              if (idx >= pixels.length) {
                println("x %d xp %d y %d yp %d idx %d ttl %d".format(x, xp, y, yp, idx, pixels.length))
              } else {
                pixels((x + xp) + (y + yp) * w) = applyColor(c, color)
              }
            }
          }
        }
      }
    }
  }

  def applyColor(o: Int, c: Int): Int = {
    val rp = (c & 0xff0000).toDouble / 0xff0000
    val gp = (c & 0x00ff00).toDouble / 0x00ff00
    val bp = (c & 0x0000ff).toDouble / 0x0000ff
    applyColor(o, rp, gp, bp)
  }

  def applyColor(o: Int, p: Double): Int = {
    applyColor(o, p, p, p)
  }

  def applyColor(o: Int, rp: Double, gp: Double, bp: Double): Int = {
    val r = ((o & 0xff0000) * rp).toLong & 0xff0000
    val g = ((o & 0x00ff00) * gp).toLong & 0x00ff00
    val b = ((o & 0x0000ff) * bp).toLong & 0x0000ff
    (r|g|b).toInt
  }

  def darken(_xp: Int, _yp: Int, percent: Double) {
    val xp = _xp - offset._1
    val yp = _yp - offset._2
    for (y <- 0 until 8) {
      if (y + yp >= 0 && y + yp < h) {
        for (x <- 0 until 8) {
          if (x + xp >= 0 && x + xp < w) {
            val c = pixels((x + xp) + (y + yp) * w)
            pixels((x + xp) + (y + yp) * w) = applyColor(c, percent)
          }
        }
      }
    }
  }
}
