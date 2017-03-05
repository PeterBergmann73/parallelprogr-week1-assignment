package scalashop

import java.util.concurrent.ForkJoinTask
import common._


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


  def parallel[A, B](ls: List[A])(f: => A => B): List[B] = {
    ls match {
      case Nil => List[B]()
      case h::t =>
        val tasks1: List[ForkJoinTask[B]] = t.map(v => task(f(v)))
        val headTask: B = f(h)
        headTask +: tasks1.map(_.join)
    }
  }

}
