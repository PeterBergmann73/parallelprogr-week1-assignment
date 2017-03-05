package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    BoxBlurUtils.checkDimensions(src, dst)

    val width = src.width
    require(from >= 0 && from < width, s"illegal from $from for width $width")
    require(end > 0 && end <= width, s"illegal end $end for height $width" )

    val height = src.height

    var yc = 0
    var xc = from
    var rgba = zeroRGBA

    while (xc < end) {
      while (yc < height) {
        rgba = boxBlurKernel(src = src, x = xc, y = yc, radius = radius)
        dst.update(x = xc, y = yc, c = rgba)

        // increment row
        yc += 1
      }

      // start a new column
      yc = 0
      xc += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    BoxBlurUtils.checkDimensions(src, dst)

    val width = src.width
    val bounds: List[(Int, Int)] = BoxBlurUtils.bounds(size = width, numTasks = numTasks)

    BoxBlurUtils.parallel(bounds) {
      case (f, e) =>
        blur(src = src, dst = dst, from = f, end = e, radius = radius)
    }
  }

}
