import breeze.linalg._
import breeze.stats.distributions._
import breeze.numerics._
import breeze.stats.mean

import scala.util.Random

trait LinkFunction {
  /* e.g. log */
  def link(mu: Double): Double
  /* e.g. exp */
  def ilink(mu: Double): Double
  /* e.g. 1/mu */
  def dlink(mu: Double): Double
}

trait Distribution {
  def variance(mu: Double): Double
}

trait Family extends LinkFunction with Distribution {
  def wwdenom(mu: Double): Double = 1. / ((dlink(mu)*dlink(mu)) * variance(mu))
}

class GenericFamily(Link:  LinkFunction, Dist:  Distribution) extends Family { 
  def ilink(mu: Double): Double = Link.ilink(mu)
  def link(mu: Double): Double = Link.link(mu)
  def dlink(mu: Double): Double = Link.dlink(mu)
  def variance(mu: Double): Double = Dist.variance(mu)
}

class PoissonLog extends Family {
  def link(mu: Double): Double = log(mu)
  def ilink(mu: Double): Double = exp(mu)
  def dlink(mu: Double): Double = 1.0/mu
  def variance(mu: Double): Double = mu
  override def wwdenom(mu: Double): Double = (1.0/(mu*mu*mu))
}

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

/*** u = exp(neta)
 *** neta = log(u)
 *** dneta/du = 1/u
****/

class GLM(family: Family) {
  def main(args: Array[String]) = {
    val N = args(0).toInt
    val p = args(1).toInt

    /* Randomly Generate the Design */
    val X = DenseMatrix.rand[Double](N,p)
    X(::,0) := DenseVector.ones[Double](N)

    /* Assume Constnat Weights for now */
    val W = DenseVector.ones[Double](N)

    /* Generate the true betas */
    val gauss = new Gaussian(0, 1)
    val betas = new DenseVector(gauss.sample(p).toArray)
    println("True Betas")
    println(betas)

    /* Generate true mean */
    val mu = exp(X * betas)

    /* Randomly simulate mean */
    val y = mu.map { Poisson(_).draw.toDouble }

    /* Working mu */
    var betaHat = DenseVector.zeros[Double](p)
    betaHat(0) = 0.05

    var iter = 0
    var n = 100.0
    while ( iter < 10000 && n >= 1e-6) {
      println(betaHat)
      val nuhat = X*betaHat
      val yhat = nuhat.map { family.ilink }
      val ww = diag(W :* yhat.map { family.wwdenom })
      val yy = nuhat + (y - yhat) :* yhat.map(family.dlink)
      val common = X.t * ww
      val xtwx = common * X 
      val xtwy = common * yy
      val betaHatNext = xtwx \ xtwy
      val diff = betaHatNext - betaHat
      n = sum(diff.map( (x) => x*x ))
      println(n)
      betaHat = xtwx \ xtwy
      iter += 1
    }
  }
}

object MyTest {
  def main(args: Array[String]) = {
        
    val test = TestFakeDesignMatrix
    test.run(args)
    /*val test = new PoissonLog()
    println("link is " + test.link(2))
    println("ilink is " + test.ilink(2))
    println("dlink is " + test.dlink(2))
    println("var is " + test.variance(2))
    println("wwdenom is " + test.wwdenom(2))
    new GLM(new PoissonLog()).main(args)
    */
  }
}
