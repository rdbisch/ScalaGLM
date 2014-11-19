import scala.math;

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
  def link(mu: Double): Double = scala.math.log(mu)
  def ilink(mu: Double): Double = scala.math.exp(mu)
  def dlink(mu: Double): Double = 1.0/mu
  def variance(mu: Double): Double = mu
  override def wwdenom(mu: Double): Double = mu
}

