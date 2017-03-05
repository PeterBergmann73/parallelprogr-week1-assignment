package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    BoxBlurUtils.checkDimensions(src, dst)

    val height = src.height
    require(from >= 0 && from < height, s"illegal from $from for height $height")
    require(end > 0 && end <= height, s"illegal end $end for height $height")

    val width = src.width

    var yc = from
    var xc = 0
    var rgba = zeroRGBA

    while (yc < end) {
      while (xc < width) {
        rgba = boxBlurKernel(src = src, x = xc, y = yc, radius = radius)
        dst.update(x = xc, y = yc, c = rgba)
        xc += 1
      }

      // start a new row
      xc = 0
      yc += 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    // TODO implement using the `task` construct and the `blur` method
    BoxBlurUtils.checkDimensions(src, dst)

    val height = src.height
    val bounds: List[(Int, Int)] = BoxBlurUtils.bounds(size = height, numTasks = numTasks)

    BoxBlurUtils.parallel(bounds) {
      case (f, e) =>
        blur(src = src, dst = dst, from = f, end = e, radius = radius)
    }
  }

}
