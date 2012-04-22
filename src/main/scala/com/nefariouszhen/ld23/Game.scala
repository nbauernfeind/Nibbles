package com.nefariouszhen.ld23

import entity.Player
import gen.World
import graphics.{SpriteSheet, Screen}
import sound.SoundLoop
import java.awt.image.{DataBufferInt, BufferedImage}
import javax.swing.JFrame
import java.awt.{BorderLayout, Dimension, Canvas}
import javax.imageio.ImageIO

object Game {
  val WIDTH = 320
  val HEIGHT = 240
  val NAME = "Nibble"
  val SCALE = 2

  def main(args: Array[String]) {
    val game = new Game()
    game.setMinimumSize(new Dimension(WIDTH * SCALE, HEIGHT * SCALE))
    game.setMaximumSize(new Dimension(WIDTH * SCALE, HEIGHT * SCALE))
    game.setPreferredSize(new Dimension(WIDTH * SCALE, HEIGHT * SCALE))

    val frame = new JFrame(Game.NAME)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(new BorderLayout())
    frame.add(game, BorderLayout.CENTER)
    frame.pack()
    frame.setResizable(false)
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)

    game.start()
  }
}

class Game extends Canvas with Runnable {

  import Game._

  private[this] var running = false

  private[this] val image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)
  private[this] val pixels = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData

  private[this] val input = new InputHandler(this)

  private[this] val screen = new Screen(WIDTH, HEIGHT, new SpriteSheet(loadImgResource("/tiles.png")))

  val world = new World()
  private[this] var player: Player = null

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
    val snd = SoundLoop.BG_LOOP
    snd.startPlaying()

    var lastTime = System.nanoTime()
    var lastTimer = System.currentTimeMillis()
    var unprocessed = 0.0d
    var nsPerTick = 1.0e9 / 60
    var frames = 0
    var ticks = 0

    init()
    resetGame()

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

  def resetGame() {
    player = new Player(this, input)
    player.x = world.dimension / 2 * 16 + 4
    player.y = world.dimension / 2 * 16 + 4

    world.generate()
    world.addPlayer(player)
  }

  def tick() {
    if (!hasFocus) {
      input.releaseAll()
      return
    }

    input.tick()
    world.tick()

    if (input.regen.down) {
      input.regen.down = false
      resetGame()
    }
  }

  def render() {
    val bs = getBufferStrategy
    val g = bs.getDrawGraphics

    val (xo, yo) = (player.x - screen.w / 2, player.y - screen.h / 2)
    world.renderBackground(screen, xo, yo)
    world.renderSprites(screen, xo, yo)
    world.renderFogOfWar(screen, player, xo, yo)

    for (y <- 0 until screen.h; x <- 0 until screen.w) {
      pixels(x + y * WIDTH) = screen.pixels(x + y * screen.w)
    }

    // Display Image
    g.fillRect(0, 0, getWidth, getHeight)

    val (ww, hh) = (getWidth * SCALE, getHeight * SCALE)
    g.drawImage(image, (getWidth - ww) / 2, (getHeight - hh) / 2, ww, hh, null)

    g.dispose()
    bs.show()
  }

  def loadImgResource(name: String) = ImageIO.read(this.getClass.getResourceAsStream(name))
}
