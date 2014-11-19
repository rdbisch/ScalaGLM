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

class IdentityLink extends LinkFunction {
  def link(mu: Double): Double = mu
  def ilink(mu: Double): Double = mu
  def dlink(mu: Double): Double = 1
}

trait Distribution {
  def variance(mu: Double): Double
}

class NormalDistribution extends Distribution {
  def variance(mu: Double): Double = 1
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
  var xtwx = DenseMatrix.zeros[Double](4,4)
  var xtwy = DenseVector.zeros[Double](4)

  def update(x: DenseVector[Double], w: Double, y: Double) = {
    for (i <- 0 until x.size) {
      val temp = x(i) * w
      for (j <- i until x.size) {
        xtwx(i, j) += temp * x(j)
      }
      xtwy(i) += temp * y
    }
  }
  
  def reflect() {
    for (i <- 0 until xtwx.rows) {
      for (j <- 0 until i) {
        xtwx(i, j) = xtwx(j, i)
      }
    }
  }

  def main(args: Array[String]) = {
    val N = args(0).toInt
    val p = args(1).toInt

    xtwx = DenseMatrix.zeros[Double](p, p)
    xtwy = DenseVector.zeros[Double](p)

    /* Randomly Generate the Design */
    val X = new FakeDesignMatrix(N, p, 295234)

    /* Assume Constnat Weights for now */
    val W = DenseVector.ones[Double](N)

    /* Generate the true betas */
    val gauss = new Gaussian(0, 1)
    //val betas = new DenseVector(gauss.sample(p).toArray)
    val betas = DenseVector.zeros[Double](p)
    for (i <- 0 until p) betas(i) = 0.05 * (p - i)
    println("True Betas")
    println(betas)

    /* Generate true mean */
    var mu = DenseVector.zeros[Double](N)
    for ((row, i) <- X.zipWithIndex) {
      mu(i) = family.ilink(row dot betas)
    }

    /* Randomly simulate response per true mean */
    val y = mu.map { Poisson(_).draw }

    /* Working mu */
    var betaHat = DenseVector.zeros[Double](p)
    betaHat(0) = 0.05

    var iter = 0
    var n = 100.0
    while ( iter < 10000 && n >= 1e-6) {
      println(betaHat)
      xtwx = DenseMatrix.zeros[Double](p, p)
      xtwy = DenseVector.zeros[Double](p)
      for ((row, i) <- X.zipWithIndex) {
        val nuhat = row dot betaHat
        val yhat = family.ilink(nuhat)
        val ww = W(i) * family.wwdenom(yhat)
        val yy = nuhat + (y(i) - yhat) * family.dlink(yhat)
        update(row, ww, yy) 
      }
      reflect()
      val betaHatNext = xtwx \ xtwy
      val diff = betaHatNext - betaHat
      n = sum(diff.map( (x) => x*x ))
      betaHat = xtwx \ xtwy
      iter += 1
    }
    println(betaHat)
  }
}

object MyTest {
  def main(args: Array[String]) = {
        
//    val test = TestFakeDesignMatrix
//    test.run(args)
    val test = new PoissonLog()
    println("link is " + test.link(2))
    println("ilink is " + test.ilink(2))
    println("dlink is " + test.dlink(2))
    println("var is " + test.variance(2))
    println("wwdenom is " + test.wwdenom(2))
    new GLM(new PoissonLog()).main(args)
    //new GLM(new GenericFamily(new IdentityLink(), new NormalDistribution())).main(args) 
  }
}
