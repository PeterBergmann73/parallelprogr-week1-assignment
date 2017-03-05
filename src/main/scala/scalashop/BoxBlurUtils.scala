package scalashop

/**
  * Created by slava on 05.03.17.
  */
object BoxBlurUtils {

  def bounds(size: Int, numTasks: Int): List[(Int, Int)] = {
    import scala.math._
    val step = min(floor(size / numTasks).toInt + 1, size)

    val res: List[(Int, Int)] = {
      val x = 0 to size
      val s0 = x.by(step).toList
      val s = if (s0.last < size) s0 :+ size else s0
      s.zip(s.tail)
    }

    res
  }


  def checkDimensions(src: Img, dst: Img): Unit = {
    require(src.width == dst.width,
      s"source width ${src.width} and destination width ${dst.width} do not match")

    require(src.height == dst.height, "different heights")
  }

}
