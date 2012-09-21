/**
   Copyright 2012 Greg L. Turnquist

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package com.turnquistwealthbuilders

import SP500._
import FinanceMath._

/* Assume all percents are given in "%" format, while absolutes are in pure decimal format */
object Main extends App {

  /** apply allows it to sort the right value into the middle, and then pick it 
   *  For example, EiulLimits(0.0, 15.0)(4.0)  would become List(0.0, 4.0, 15.0), with 4.0 being in the middle
   *               EiulLimits(0.0, 15.0)(-2.4  would become List(-2.4, 0.0, 15.0), with 0.0 being in the middle
   *               EiulLimits(0.0, 15.0)(22.5) would become List(0.0, 15.0, 22.5), with 15.0 being in the middle
   */
  case class EiulLimits(lower:Double, upper:Double) {
    def apply(x: Double) = List(x, lower, upper).sorted.apply(1)
  }

  def eiul(xs: Seq[(Int, Double)], limits: EiulLimits): Seq[(Int, Double)] = {
    xs.map { case(year, relChange) => (year, limits(relChange)) }
  }

  def series(xs: Seq[(Int, Double)], years: Int) = {
    xs.sliding(years).map(sublist => 
      (sublist(0)._1, sublist.takeRight(1)(0)._1, aMean(sublist), gMean(sublist), sublist)
    ).toList
  }

  /* Need a better container for the statistical data than a plain Map */
  case class StatsData(val avgGeomMean:Double, val minGeomMean: Double, val maxGeomMean: Double, val stddev: Double,
                       val minList: (Int, Int, Double, Double, Seq[(Int, Double)]), 
                       val maxList: (Int, Int, Double, Double, Seq[(Int, Double)]))

  def calc_stats(xs: Seq[(Int, Int, Double, Double, Seq[(Int, Double)])]) = {
    val aMeans = xs.map(_._3)
    val gMeans = xs.map(_._4)
    val minList = xs.tail.foldLeft(xs.head)((min, item) => if (gMean(item._5) < gMean(min._5)) item else min)
    val maxList = xs.tail.foldLeft(xs.head)((max, item) => if (gMean(item._5) > gMean(max._5)) item else max)
    StatsData(round(gMeans.sum/xs.size, 2), 
        round(gMeans.min, 2), 
        round(gMeans.max, 2),
        stddev(gMeans),
        minList,
        maxList)
  }

  println("S&P 500 performance = " + snp)
  println("Arithmetic mean = " + aMean(snp) + "%")
  println("Geometric mean = " + gMean(snp) + "%" )
  println("Actual total growth factor = " + actualAbsGrowth(snp))
  println

  val eiulData = eiul(snp, EiulLimits(0.0, 15.0))

  println("EIUL performance = " + eiulData)
  println("EIUL arithmetic performance = " + aMean(eiulData) + "%")
  println("EIUL geometric performance = " + gMean(eiulData) + "%")
  println("Actual EIUL total growth factor = " + actualAbsGrowth(eiulData))
  println

  println("""
  This is where every 10, 15, 20, etc. year interval in all the data is evaluated and then averaged together.
  The min and max performance of each interval is displayed, and the 1st stddev is shown. There is a 68%
  that actual performance is within 1 stddev of the average.
  """)

  List(10, 15, 20, 25, 30).foreach {interval =>
    val snpStats = calc_stats(series(snp, interval))
    val eiulStats = calc_stats(series(eiulData, interval))
    println(interval + "-year stats")
    println("==========================")
    List(("S&P 500", snpStats), ("EIUL", eiulStats)).foreach {
      _ match {
        case (desc, stats) => println("%s stats:\t Avg geom mean = %.2f (%.2f..%.2f)\t68%% chance between %.2f and %.2f".format(
                                      desc, 
                                      stats.avgGeomMean, 
                                      stats.minGeomMean, 
                                      stats.maxGeomMean, 
                                      stats.avgGeomMean - stats.stddev, 
                                      stats.avgGeomMean + stats.stddev))
                               println("Windows displayed are (first year, last year, arithmetic mean, geom mean, and the original data)")
                               println("Min window is " + stats.minList)
                               println("Max window is " + stats.maxList)
                               println()
      }
    }
    println
  }
}
