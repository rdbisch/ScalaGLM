package org.rbisd.glm;
/** Represents the distribution to be used in the GLM family */
trait Distribution {
  def variance(mu: Double): Double
}

class NormalDistribution extends Distribution {
  def variance(mu: Double): Double = 1
}

class PoissonDistribution extends Distribution {
  def variance(mu: Double): Double = mu
}

class GammaDistribution extends Distribution {
  def variance(mu: Double): Double = mu*mu
}

class TweedieDistribution(p: Double) extends Distribution {
  def variance(mu: Double): Double = scala.math.pow(mu, p)
}

