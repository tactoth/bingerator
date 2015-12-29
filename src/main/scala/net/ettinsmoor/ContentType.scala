package net.ettinsmoor

import java.util.regex.Pattern

abstract class ContentType

case class ImageJPEG() extends ContentType

case class ImagePNG() extends ContentType

case class ImageGIF() extends ContentType

case class ImageOther(mimeType: String) extends ContentType

object ContentType {
  val patternGif = Pattern.compile("^image/(animated)?gif$")
  val patternJpeg = Pattern.compile("^image/jpe?g$")
  val patternPng = Pattern.compile("^image/png$")

  def matches(str: String, pattern: Pattern) = pattern.matcher(str).matches()

  def get(ct_str: String) = ct_str match {
    case gif if matches(gif, patternGif) => ImageGIF
    case jpeg if matches(jpeg, patternGif) => ImageJPEG
    case png if matches(png, patternGif) => ImagePNG
    case _ => ImageOther(ct_str)
  }
}