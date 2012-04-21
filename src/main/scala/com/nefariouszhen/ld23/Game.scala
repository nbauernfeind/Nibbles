package com.nefariouszhen.ld23

import gen.{Tile, World}
import java.awt.Canvas
import sound.SoundLoop
import java.awt.image.{DataBufferInt, BufferedImage}

object Game {
  val WIDTH = 160
  val HEIGHT = 120
  val NAME = "Nibble"
  val SCALE = 3
}

class Game extends Canvas with Runnable {
  import Game._

  private[this] var running = false

  private[this] val image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)
  private[this] val pixels = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData

  private[this] val world = new World()

  def start() {
    running = true
    new Thread(this).start()
  }

  def stop() {
    running = false
    SoundLoop.stopPlaying()
    Thread.sleep(500)
  }

  def run() {
//    val snd = new SoundLoop("/ld_bg_loop.wav")
//    snd.startPlaying()
    var lastTime = System.nanoTime()
    var lastTimer = System.currentTimeMillis()
    var unprocessed = 0.0d
    var nsPerTick = 1.0e9 / 60
    var frames = 0
    var ticks = 0

    init()

    while (running) {
      val now = System.nanoTime()
      unprocessed += (now - lastTime) / nsPerTick
      lastTime = now
      var shouldRender = true

      while (unprocessed >= 1) {
        ticks += 1
        tick()
        unprocessed -= 1
        shouldRender = true
      }

      Thread.sleep(2)
      if (shouldRender) {
        frames += 1
        render()
      }

      if (System.currentTimeMillis() - lastTimer > 1000) {
        lastTimer += 1000
        println(ticks + " ticks, " + frames + " fps")
        frames = 0
        ticks = 0
      }
    }
  }

  def init() {
    createBufferStrategy(3)
    requestFocus()
  }

  def tick() {

  }

  def render() {
    val bs = getBufferStrategy
    val g = bs.getDrawGraphics

    // Update Image
    val (cw,ch) = ((WIDTH - world.dimension)/2,(HEIGHT - world.dimension)/2)
    for (x <- 0 until WIDTH; y <- 0 until HEIGHT) {
//      pixels(x + y * WIDTH) = ((HEIGHT-y).toDouble / HEIGHT * 0xFF).toInt
      pixels(x + y * WIDTH) = world.getTile(x-cw,y-ch) match {
        case Tile.EMPTY => 0x0000FF
        case Tile.WALL => 0x00FF00
        case Tile.FLOOR => 0xFF0000
      }
    }

    // Display Image
    g.fillRect(0, 0, getWidth, getHeight)

    val (ww,hh) = (getWidth * SCALE, getHeight * SCALE)
    g.drawImage(image, (getWidth - ww)/2, (getHeight - hh)/2, ww, hh, null)

    g.dispose()
    bs.show()
  }
}
