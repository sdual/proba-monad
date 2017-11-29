package stanz.sandbox.modeling

import java.io.{FileOutputStream => FileStream, OutputStreamWriter => StreamWriter}

import stanz.sandbox.modeling.algorithm.MetropolisHastings
import stanz.sandbox.modeling.distribution.DistributionInstances._
import stanz.sandbox.modeling.distribution._
import org.apache.commons.math3.distribution.NormalDistribution

import scala.util.Random
import scalaz.Scalaz._

object TestMH extends App {

  val mh = new MetropolisHastings

  val r = new Random

  val n = 100000
  //val data: List[Int] = List(0, 1, 0, 0, 2, 0, 1, 0, 0, 1)

  //val poi = new PoissonDistribution(3.2)
  //def likelihood(x: Int): Double = poi.probability(x)

  //println(trainingData())

  val ps = points(trainingData(), linear())
  //println(trainingData().map(x => s"${x._1},${x._2}").mkString("\n"))
  println(trainingData())

  val sampled = (for {
    result <- mh.run(n, ps)
  } yield result)
    .sample(r)

  //println(sampled.map(x => x._1).mkString("\n"))

  val fileName = "/Users/tenoritama/git/bayesian-freaks/stanz/result.csv"
  val fileOutPutStream = new FileStream(fileName, true)
  val writer = new StreamWriter(fileOutPutStream, "UTF-8")
  writer.write("a,b\n")
  for {
    line <- sampled.reverse.map(x => s"${x._1},${x._2}").mkString("\n")
  } yield writer.write(line)

  writer.close()

  /*(for {
    result <- linear()
  } yield println(result))
    .sample(r)*/

  def point(data: (Double, Double), dist: Distribution[(Double, Double)]): Distribution[(Double, Double)] = {
    def func(param: (Double, Double)): Probability = {
      val norm = new NormalDistribution(param._1 * data._1 + param._2, 1.0)
      norm.density(data._2)
    }
    Conditional(dist, func)
  }

  def points(data: List[(Double, Double)], dist: Distribution[(Double, Double)]): Distribution[(Double, Double)] = {
    data.foldLeft(dist){(u, v) => point(v, u)}
  }

  def trainingData(): List[(Double, Double)] = {
    val points = (0.0 to 2.0 by 1.0).toList
    def equation(x: Double): Double = -0.5 * x + 0.3
    def addNoise(x: Double): Double = (new Random).nextGaussian() * 0.2 + x
    points.map { x =>
      (x, addNoise(equation(x)))
    }
  }

  def linear(): Distribution[(Double, Double)] = {

    def normal(mean: Double, stdDev: Double): Distribution[Double] = Primitive(new Normal(mean, stdDev))

    for {
      a <- normal(0.0, 1.0)
      b <- normal(0.0, 1.0)
    } yield (a, b)
  }

}
