package com.nefariouszhen.ld23

import java.applet.Applet
import java.awt.BorderLayout

class GameApplet extends Applet {

  private[this] val game = new Game()

  override def init() {
    setLayout(new BorderLayout)
    add(game, BorderLayout.CENTER)
  }

  override def start() {
    game.start()
  }

  override def stop() {
    game.stop()
  }
}
