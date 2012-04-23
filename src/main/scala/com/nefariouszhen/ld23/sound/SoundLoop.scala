package com.nefariouszhen.ld23.sound

import java.util.concurrent.atomic.AtomicBoolean
import javax.sound.sampled.{Clip, FloatControl, AudioSystem}

trait SoundLoop {
  def startPlaying()
  def stopPlaying()
}

object SoundLoop {
  val BG_LOOP: SoundLoop = new SoundLoopImpl("ld_bg_loop.wav")
  val BG_LOOP2: SoundLoop = new SoundLoopImpl("deep bass.wav")
  val BG_LOOP3: SoundLoop = new SoundLoopImpl("ld23-lp3.wav")

  val BG = List(BG_LOOP,BG_LOOP2,BG_LOOP3)

  def stopPlaying() {
    List(BG).flatten.foreach(_.stopPlaying())
  }
}

// Start/Transition/Stop Duration (all different) Min/Max probably fixed.
//private class SoundLoopImpl(name: String, min: Float = -20.0f, max: Float = -05.0f, durationInMs: Int = 2200) extends SoundLoop {
private class SoundLoopImpl(name: String, min: Float = -20.0f, max: Float = -15.0f, durationInMs: Int = 2200) extends SoundLoop {
  private[this] val audio = AudioSystem.getAudioInputStream(this.getClass.getResource(name))
  private[this] val playing = new AtomicBoolean(false)

  def startPlaying() {
    if (playing.compareAndSet(false, true)) {
      new Thread() {
        override def run() {
          val startTime = System.currentTimeMillis()

          val clip = AudioSystem.getClip
          val volume = clip.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
          volume.setValue(min)

          clip.open(audio)
          volume.setValue(min)
          Thread.sleep(50)
          clip.loop(Clip.LOOP_CONTINUOUSLY)

          while (playing.get()) {
            volume.setValue(math.min(max, (max - min) / durationInMs * (System.currentTimeMillis() - startTime) + min))
            Thread.sleep(50)
          }

          val stopTime = System.currentTimeMillis()
          while (volume.getValue > min + 0.001) {
            volume.setValue(math.min(max, (max - min) / (durationInMs / 4) * ((durationInMs / 4) - (System.currentTimeMillis() - stopTime)) + min))
            Thread.sleep(50)
          }

          clip.stop()
        }
      }.start()
    }
  }

  def stopPlaying() {
    playing.set(false)
  }
}
