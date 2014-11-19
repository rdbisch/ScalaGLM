package org.rbisd.glm;

/** Represents the combination of the Link and Distribution as a pair */
trait Family extends LinkFunction with Distribution {
  def wwdenom(mu: Double): Double = 1. / ((dlink(mu)*dlink(mu)) * variance(mu))
}

/** A way to compose known links and distributions */
class GenericFamily(Link:  LinkFunction, Dist:  Distribution) extends Family { 
  def ilink(mu: Double): Double = Link.ilink(mu)
  def link(mu: Double): Double = Link.link(mu)
  def dlink(mu: Double): Double = Link.dlink(mu)
  def variance(mu: Double): Double = Dist.variance(mu)
}

/** Hard coded alternative for Poisson-Log
  * Note in particular that wwdenom is simplified, thereby gaining some efficiency.
  */
class PoissonLog extends Family {
  def link(mu: Double): Double = scala.math.log(mu)
  def ilink(mu: Double): Double = scala.math.exp(mu)
  def dlink(mu: Double): Double = 1.0/mu
  def variance(mu: Double): Double = mu
  override def wwdenom(mu: Double): Double = mu
}

