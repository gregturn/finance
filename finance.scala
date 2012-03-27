/* Assume all percents are given in "%" format, while absolutes are in pure decimal format */
object Main extends App {

  val snp = List(
    (1971, 10.8), (1972, 15.6), (1973, -17.4), (1974, -29.7), (1975, 31.5),
    (1976, 19.1), (1977, -11.5), (1978, 1.1), (1979, 12.3), (1980, 25.8),
    (1981, -9.7), (1982, 14.8), (1983, 17.3), (1984, 1.4), (1985, 26.3),
    (1986, 14.6), (1987, 2.0), (1988, 12.4), (1989, 27.3), (1990, -6.6), 
    (1991, 26.3), (1992, 4.5), (1993, 7.1), (1994, -1.5), (1995, 34.1),
    (1996, 20.3), (1997, 31.0), (1998, 26.7), (1999, 19.5), (2000, -10.1),
    (2001, -13.0), (2002, -23.4), (2003, 26.4), (2004, 9.0), (2005, 3.0),
    (2006, 13.6), (2007, 3.5), (2008, -38.5), (2009, 23.5), (2010, 12.8))

  def round(x:Double, digits:Int):Double = {
    val factor = math.pow(10, digits)
    math.round(x * factor) / factor
  }

  def aMean(xs: Seq[(Int, Double)]): Double = xs.foldLeft(0.0)((subtotal, relChange) => subtotal + relChange._2) / xs.size

  def absChange(relChange:Double) = 1 + relChange/100.0
  def relChange(absChange:Double) = 100.0 * (absChange - 1.0)
  
  def actualAbsGrowth(xs: Seq[(Int, Double)]): Double = xs.foldLeft(1.0)((subtotal, relChange) => subtotal * absChange(relChange._2))

  def gMean(xs: Seq[(Int, Double)]): Double = {
    relChange(math.pow(actualAbsGrowth(xs), 1.0/xs.size))
  }

  case class EiulLimits(lower:Double, upper:Double)

  def eiul(xs: Seq[(Int, Double)], limits:EiulLimits): Seq[(Int, Double)] = {
    xs.map(item => (item._1, if (item._2 < limits.lower) limits.lower else if (item._2 > limits.upper) limits.upper else item._2))
  }

  println("S&P 500 performance = " + snp)
  println("Arithmetic mean = " + aMean(snp) + "%")
  println("Geometric mean = " + gMean(snp) + "%" )
  println("Actual total growth factor = " + actualAbsGrowth(snp))
  println
  println("EIUL performance = " + eiul(snp, EiulLimits(0.0, 15.0)))
  println("EIUL arithmetic performance = " + aMean(eiul(snp, EiulLimits(0.0, 15.0))) + "%")
  println("EIUL geometric performance = " + gMean(eiul(snp, EiulLimits(0.0, 15.0))) + "%")
  println("Actual EIUL total growth factor = " + actualAbsGrowth(eiul(snp, EiulLimits(0.0, 15.0))))

}
