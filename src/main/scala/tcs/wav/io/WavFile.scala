package tcs.wav.io

import java.io.{ByteArrayInputStream, File}
import javax.sound.sampled.{AudioFileFormat, AudioFormat, AudioInputStream}

import com.sun.media.sound.{WaveFileReader, WaveFileWriter}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by t.i.kirillova on 14.09.2016.
  */
class WavFile {

  /**
    *
    * @param wavFile - wav file
    * @return - signal in array of integers
    */
  def readFromFile(wavFile: File): Array[Int]= {

    val audioInputStream: AudioInputStream = new WaveFileReader()
      .getAudioInputStream(
        wavFile
      )

    implicit val audioFormat = audioInputStream.getFormat

    (audioFormat.getFrameSize, audioFormat.getEncoding) match {
      case (2, AudioFormat.Encoding.PCM_SIGNED) => read2BitsPCM(audioInputStream)
      case (1, AudioFormat.Encoding.PCM_SIGNED) => read1bitsPCM(audioInputStream)
      case _ => throw new Exception(s"Format $audioFormat isn't yet support")
    }

  }

  private def read2BitsPCM(ais: AudioInputStream)(implicit audioFormat: AudioFormat): Array[Int] = {
    val buf = new Array[Byte](audioFormat.getFrameSize)

    var amount = 0
    val xss = ArrayBuffer[Byte]()
    do {
      amount = ais.read(buf)
      xss.append(buf(0))
      xss.append(buf(1))
    } while (amount > -1)

    (xss.grouped(2) map { b =>
      (b(0) & 0xff) | b(1).toInt << 8
    }).toArray
  }

  private def read1bitsPCM(ais: AudioInputStream)(implicit audioFormat: AudioFormat) = {
    Iterator.continually(ais.read)
      .takeWhile(_ != -1).toArray
  }

  /**
    * Write wav file 1 bits 8000 Ghz
    * @param data - data of sound
    * @param outFile - output file
    * @return
    */
  def write1BitsPCM(data: Array[Byte], outFile: File) = {
    write(data, outFile, new AudioFormat(8000, 8, 1, true, false))
  }

  /**
    * Write wav file 2 bits 8000 Ghx
    * @param data - array of integers (1 integers coding by 2 bytes)
    * @param outFile - output wav file
    * @return
    */
  def write2BitsPCM(data: Array[Int], outFile: File) = {
    val dataBytes = data.flatMap { i =>
      Array((i & 0xFF).toByte, ((i & 0xFF00) >> 8).toByte)
    }

    write(dataBytes, outFile, new AudioFormat(8000, 16, 1, true, false))
  }

  private def write(data: Array[Byte], outFile: File, audioFormat: AudioFormat) = {
    val wf = new WaveFileWriter()
    val al = new AudioInputStream(
      new ByteArrayInputStream(data),
      audioFormat,
      data.length
    )
    wf.write(al, AudioFileFormat.Type.WAVE, outFile)
  }

}
