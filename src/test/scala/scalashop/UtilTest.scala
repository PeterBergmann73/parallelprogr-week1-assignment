package scalashop

import org.scalatest.FunSpec


/**
  * Created by slava on 05.03.17.
  */
class UtilTest extends FunSpec {

  it("data is modified"){

    val radius = 3
    val width = 10
    val height = 30
    val multiplier = 1000000
    val data: Array[RGBA] = (1 to (width * height)).toArray.map(_ * multiplier)
    val src = new Img(width, height, data.clone)
    val dst = new Img(width, height, data.clone)
    VerticalBoxBlur.blur(src, dst, 0, width, radius)

    assert(src(0, 0) == data(0))
    assert(dst(0, 0) != data(0))
  }

  it("correct bounds") {

    val size = 32
    val numTasks: List[Int] = (1 to size).toList

    numTasks.foreach {
      n =>
        val b = BoxBlurUtils.bounds(size = size, numTasks = n)
        require(b.length == n, s"number of tasks $n, number of bounds ${b.length}")
    }

  }

}
