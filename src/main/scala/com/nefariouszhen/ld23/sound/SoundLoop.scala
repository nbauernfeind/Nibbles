package com.nefariouszhen.ld23.sound

import java.util.concurrent.atomic.AtomicBoolean
import java.io.BufferedInputStream
import javax.sound.sampled.{AudioFormat, Clip, FloatControl, AudioSystem}

trait SoundLoop {
  def startPlaying()
  def stopPlaying()
}

object SoundLoop {
  val BG_LOOP: SoundLoop = new SoundLoopImpl("ld_bg_loop.mp3")
  val BG_LOOP2: SoundLoop = new SoundLoopImpl("deep bass.mp3")
  val BG_LOOP3: SoundLoop = new SoundLoopImpl("ld23-lp3.mp3")

  val BG = List(BG_LOOP, BG_LOOP2, BG_LOOP3)

  def stopPlaying() {
    List(BG).flatten.foreach(_.stopPlaying())
  }
}

// Start/Transition/Stop Duration (all different) Min/Max probably fixed.
private class SoundLoopImpl(name: String, min: Float = -20.0f, max: Float = -05.0f, durationInMs: Int = 2200) extends SoundLoop {
  //private class SoundLoopImpl(name: String, min: Float = -15.0f, max: Float = 10.0f, durationInMs: Int = 2200) extends SoundLoop {
  private[this] val audio = {
    val resource = this.getClass.getResource(name)
    val audioStream = AudioSystem.getAudioInputStream(resource)
    val audioFormat = audioStream.getFormat

    val newFormat = new AudioFormat(
      AudioFormat.Encoding.PCM_SIGNED,
      audioFormat.getSampleRate,
      16,
      audioFormat.getChannels,
      audioFormat.getChannels * 2,
      audioFormat.getSampleRate,
      false
    )

    AudioSystem.getAudioInputStream(newFormat, audioStream)
  }

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

  /**
   * If the BufferedInputStream references input that contains an ID3v2
   * metadata tag, determine the length of the tag and skip past it.
   *
   * @param audioStream the BufferedInputStram to read from and possibly
   *                    skip in
   * @throws IOException if IO operation fails
   */
  private[this] def possiblySkipID3(audioStream: BufferedInputStream) {
    audioStream.mark(10);
    val header = Array.ofDim[Byte](10);
    if (audioStream.read(header) != header.length) {
      throw new Error("read failed");
    }
    audioStream.reset();
    if (header(0) == 'I' && header(1) == 'D' && header(2) == '3') {
      // tag length does not include the 10 byte header
      var toSkip: Long = getTagLength(header) + 10;
      toSkip -= audioStream.skip(toSkip);
      while (toSkip > 0) {
        toSkip -= audioStream.skip(toSkip);
      }
    }
  }

  private[this] def getTagLength(tagHeader: Array[Byte]): Int = {
    var i = tagHeader(6) << 21;
    i += tagHeader(7) << 14;
    i += tagHeader(8) << 7;
    i + tagHeader(9);
  }
}
