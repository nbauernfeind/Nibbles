package com.nefariouszhen.ld23.graphics

object Font {
  val tileOffset = 0 + 13*20
  val frameOffset = 0 + 12*20
  val chars = "ABCDEFGHIJKLMNOPQRST" + "UVWXYZ0123456789()<>"

  def draw(msg: String, screen: Screen, x: Int, y: Int, c: Int = 0xffffff) {
    val s = msg.toUpperCase
    for (i <- 0 until s.length) {
      val idx = chars.indexOf(s.charAt(i))
      if (idx >= 0) {
        screen.render(x + i * 8, y, idx + tileOffset, 0, c)
      }
    }
  }

  case class FrameTile(x: Int, y: Int, o: Int, f: Int) {
    def matches(a: Int, b: Int): Boolean = {
      (x == -1 || a == x) && (y == -1 || b == y)
    }
  }

  def renderFrame(screen: Screen, title: String, x0: Int, y0: Int, x1: Int, y1: Int) {
    val frames = List(
      FrameTile(x0,y0,0,0),
      FrameTile(x1,y0,0,1),
      FrameTile(x0,y1,0,2),
      FrameTile(x1,y1,0,3),
      FrameTile(-1,y0,1,0),
      FrameTile(-1,y1,1,2),
      FrameTile(x0,-1,2,0),
      FrameTile(x1,-1,2,1)
    )

    for (y <- y0 to y1; x <- x0 to x1) {
      frames.find(_.matches(x,y)) match {
        case Some(frame) => screen.render(x * 8, y * 8, frame.o + frameOffset, frame.f)
        case None => screen.darken(x * 8, y * 8, 0x111111)
      }
    }
  }
}
