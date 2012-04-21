package com.nefariouszhen.ld23

import java.awt.Canvas
import sound.SoundLoop

class Game extends Canvas with Runnable {
  private[this] var running = false

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
    val snd = new SoundLoop("/ld_bg_loop.wav")
    snd.startPlaying()

    while(running) {
      Thread.sleep(5000)
//      snd.stopPlaying()
    }
  }
}
