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

import SP500._
import FinanceMath._
import Math._

/* Assume all percents are given in "%" format, while absolutes are in pure decimal format */
object DollarCostAveraging extends App {

  def stockPrice(year: Int): Double = {
	  var subtotal = 10.0 // assume stock price at the beginning was $10.00/share
	  for (item <- snp) {
		  if (item._1 <= year) {
				subtotal = subtotal * absChange(item._2)
			}
		}
		subtotal
	}

  case class InvestingResults(val shares: Double, val cash: Double, val desc: String, val contributions: Double)

  def buy(reason: String, amount: Double, subtotal: InvestingResults, item: (Int, Double)): InvestingResults = {
	  val price = stockPrice(item._1)
    val moreShares = floor((amount + subtotal.cash) / price)
    val leftOverCash = amount + subtotal.cash - moreShares*price
    val results = InvestingResults(subtotal.shares + moreShares, leftOverCash, subtotal.desc, subtotal.contributions + amount)
    println(item._1 +
	    ": " + reason +
			" so I am investing $" + moreShares*price + 
			" when the price is " + price + 
			" giving me " + moreShares + 
			" more shares, to add to my existing net worth of $" + results.shares*price)
		results
	}
	
  def dca(xs: Seq[(Int, Double)], amount: Double): InvestingResults = {
    xs.foldLeft(InvestingResults(0.0, 0.0, "Dollar cost averaging", 0.0)) {(subtotal, item) => 
    	buy("Annual purchase", amount, subtotal, item)
    }
  }

  def investingDuringDips(xs: Seq[(Int, Double)], amount: Double): InvestingResults = {
    xs.foldLeft(InvestingResults(0.0, 0.0, "Smart investing", 0.0)) {(subtotal, item) =>
      if (item._2 >= 0.0) {
	      // Buy amount is $0.00, meaning I'm not really buying
	     	buy("Price has risen by " + item._2 + "%", 0.0, InvestingResults(subtotal.shares, 0.0, subtotal.desc, 0.0), item)
        InvestingResults(subtotal.shares, subtotal.cash + amount, subtotal.desc, subtotal.contributions + amount)
      } else {
	      buy("Price has dropped by " + item._2 + "%", amount, subtotal, item)
      }
    }
  }

  def investOnOddYears(xs: Seq[(Int, Double)], amount: Double): InvestingResults = {
    xs.foldLeft(InvestingResults(0.0, 0.0, "Odd-year investing", 0.0)) {(subtotal, item) =>
      if (item._1 % 2 == 0) {
	      // Buy amount is $0.00, meaning I'm not really buying
	     	buy("It's an even year ", 0.0, InvestingResults(subtotal.shares, 0.0, subtotal.desc, 0.0), item)
        InvestingResults(subtotal.shares, subtotal.cash + amount, subtotal.desc, subtotal.contributions + amount)
      } else {
	      buy("It's an odd year ", amount, subtotal, item)
      }
    }
  }

  def investOnEvenYears(xs: Seq[(Int, Double)], amount: Double): InvestingResults = {
    xs.foldLeft(InvestingResults(0.0, 0.0, "Even-year investing", 0.0)) {(subtotal, item) =>
      if (item._1 % 2 == 1) {
	      // Buy amount is $0.00, meaning I'm not really buying
	     	buy("It's an odd year ", 0.0, InvestingResults(subtotal.shares, 0.0, subtotal.desc, 0.0), item)
        InvestingResults(subtotal.shares, subtotal.cash + amount, subtotal.desc, subtotal.contributions + amount)
      } else {
	      buy("It's an even year ", amount, subtotal, item)
      }
    }
  }

  def skipHighsBuyOnLows(xs: Seq[(Int, Double)], amount: Double): InvestingResults = {
    xs.foldLeft(InvestingResults(0.0, 0.0, "Skip highs/buy lows investing", 0.0)) {(subtotal, item) =>
      if (item._2 > 10.0) {
	      // Buy amount is $0.00, meaning I'm not really buying
	     	buy("It's an odd year ", 0.0, InvestingResults(subtotal.shares, 0.0, subtotal.desc, 0.0), item)
        InvestingResults(subtotal.shares, subtotal.cash + amount, subtotal.desc, subtotal.contributions + amount)
      } else {
	      buy("It's an even year ", amount, subtotal, item)
      }
    }
  }

  List(
		dca(snp, 1000.0), investingDuringDips(snp, 1000.0), 
		investOnOddYears(snp, 1000.0), investOnEvenYears(snp, 1000.0),
		skipHighsBuyOnLows(snp, 1000.0)).foreach { outcome =>
			
	  println
	  val netWorth = stockPrice(snp.last._1) * outcome.shares
	  println(outcome.desc + " performance = " + outcome)
	  println("Dollars invested = " + outcome.contributions)
	  println("Net worth = " + netWorth)
	  println("Annualized ROI = " + relChange(pow(netWorth/outcome.contributions, 1.0/snp.length)) + "%")
	}
	
  println

  {
    println("What if I just bought it all up front?")
    val price = stockPrice(snp.first._1)
    val shares = floor(60000.0 / price)
    val leftOverCash = 60000.0 - shares*price
    val outcome = InvestingResults(shares, leftOverCash, "Lump sum investment", 60000.0)
    val netWorth = stockPrice(snp.last._1) * outcome.shares
    println(outcome.desc + " performance = " + outcome)
    println("Dollars invested = " + outcome.contributions)
    println("Net worth = " + netWorth)
    println("Annualized ROI = " + relChange(pow(netWorth/outcome.contributions, 1.0/snp.length)) + "%")
  }

  /*
  println("S&P 500 performance = " + snp)
  println
  val annualSavings = 500.0 * 12
  val inflation = 4.0
  List(dca(snp, annualSavings), investingDuringDips(snp, annualSavings)).foreach { results =>
    println
    val totalValue = results.shares * stockPrice(snp.last._1) + results.cash
    println(results.desc + " at $" + annualSavings + "/year = " + results.shares + " shares and $" + results.cash + " cash for a total value of $" + round(totalValue, 2))
    println("Total contributions = $" + results.contributions)
    val totalGrowth = totalValue / results.contributions
    val annualizedGrowth = math.pow(totalGrowth, 1.0/snp.size)
    println("Total growth = " + totalGrowth + "%")
    println("Annualized growth = " + relChange(annualizedGrowth) + "%")
    println
  }
  */

}
