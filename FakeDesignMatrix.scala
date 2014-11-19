import breeze.linalg._
import scala.util.Random

class FakeDesignMatrix(N: Int, p: Int, seed: Long) extends Iterable[DenseVector[Double]] {
  def iterator = new Iterator[DenseVector[Double]] {
    val rng = new Random(seed)
    var n : Int = 0
    def hasNext() : Boolean = n < N
    def next() : DenseVector[Double] = {
      var x = DenseVector.zeros[Double](p)
      x(0) = 1
      for (i <- 1 until p) x(i) = rng.nextGaussian()
      n = n + 1
      return x
    }
  }
}

object TestFakeDesignMatrix {
  def run(args: Array[String]) {
    val fdm = new FakeDesignMatrix(10000,10,12345)
    var totals = DenseVector.zeros[Double](10)
    
    for (row <- fdm) {
      for (i <- 0 until 10)
        totals(i) += row(i)
    }
    println(totals)

    val fdm2 = new FakeDesignMatrix(10000,10,12345)
    var totals2 = DenseVector.zeros[Double](10)
    
    for (row <- fdm2) {
      for (i <- 0 until 10)
        totals2(i) += row(i)
    }
    println(totals2)

    val fdm3 = new FakeDesignMatrix(10000,10,12346)
    var totals3 = DenseVector.zeros[Double](10)
    
    for (row <- fdm3) {
      for (i <- 0 until 10)
        totals3(i) += row(i)
    }
    println(totals3)
  }
}

