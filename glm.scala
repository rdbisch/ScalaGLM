import breeze.linalg._
import breeze.stats.distributions._
import breeze.numerics._
import breeze.stats.mean

object GLM {
  /* Functions around distribution and link */
  def variance(mu: Double) = mu
  def ilink(mu: Double) = log(mu)
  def link(mu: Double) = exp(mu)
  def dlink(mu: Double) = exp(mu)
  def wwdenom(mu: Double) = mu * (dlink(mu)*dlink(mu))

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
      val yhat = exp(nuhat)
      /* Compute X'WX */
      val ww = diag(W / yhat.map { wwdenom })
      val yy = nuhat + (y - yhat) :* yhat.map(dlink)
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

