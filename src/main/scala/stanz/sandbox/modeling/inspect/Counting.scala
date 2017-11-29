package stanz.sandbox.modeling.inspect

/**
  * Created by tenoritama on 1/20/17.
  */

object Counting {

  def normal (n : Int) : Int =
    if (n == 0) 0
    else 1 + normal(n - 1)

  def cps (n : Int) : Int = {
    def loop (i : Int, k : Int => TailRec[Int]) : TailRec[Int] =
      if (i == 0) k(0)
      else loop(i - 1, x => Suspend(() => k(1 + x)))

    loop(n, t => Return(t)).run
  }

  def accum (n : Int) : Int = {
    def loop (i : Int, a : Int) : Int =
      if (i == 0) a
      else loop(i - 1, a + 1)

    loop(n, 0)
  }

  def main(args : Array[String]) : Unit = {
    val n = 100000
    val s1 = System.currentTimeMillis()
    accum(n)
    println(System.currentTimeMillis() - s1)

    val s2 = System.currentTimeMillis()
    cps(n)
    println(System.currentTimeMillis() - s2)

    val s3 = System.currentTimeMillis()
    normal(n)
    println(System.currentTimeMillis() - s3)
  }
}
