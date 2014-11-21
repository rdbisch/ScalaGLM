package org.rbisd.glm;
import breeze.linalg._
import breeze.stats.distributions._
import breeze.numerics._
import breeze.stats.mean
import scala.util.Random

import org.apache.avro._
import org.apache.avro.file._
import org.apache.avro.generic._
import java.io.File
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

class Dataset {
  def test() = {
    var schema = new Schema.Parser().parse(new File("allstate_kaggle.avsc"));
    for (field <- schema.getFields()) {
      println(field.toString)
    }

    var datumReader = new GenericDatumReader[GenericRecord](schema);
    var dataFileReader = new DataFileReader[GenericRecord](new File("train_set.avro"), datumReader)
    var row : GenericRecord = null;
    var results = HashMap.empty[Object,(Long, Double)]
    while (dataFileReader.hasNext) {
      row = dataFileReader.next(row);

      val my = row.get("Model_Year")
      val t = ( 1L, row.get("Claim_Amount").asInstanceOf[Double] ) 

      if (!results.contains(my))
        results += my -> t
      else {
        val u = results(my)
        val v = ( t._1+u._1, t._2 + u._2 ) 
        results += my -> v
      }
    }

    println(results)
  }

  def test2() = {
    var datumReader = new SpecificDatumReader[allstate_kaggle](allstate_kaggle.class);
    var dataFileReader = new DataFileReader[allstate_kaggle](new File("train_set.avro"), datumReader)
    var row : allstate_kaggle = null;
    var results = HashMap.empty[Object,(Long, Double)]
    while (dataFileReader.hasNext) {
      row = dataFileReader.next(row);

      val my = row.getModelYear
      val t = ( 1L, row.getClaimAmount.toDouble ) 

      if (!results.contains(my))
        results += my -> t
      else {
        val u = results(my)
        val v = ( t._1+u._1, t._2 + u._2 ) 
        results += my -> v
      }
    }

    println(results)
  }
}

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
      betaHat = betaHatNext
      iter += 1
    }
    println(betaHat)
  }
}

object MyTest {
  def main(args: Array[String]) = {

    val test2 = new Dataset()
    //test2.test()
    test2.test2()

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
