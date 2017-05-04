
object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {

    // Define calls and puts
    val call = Option call 110 expiration 30 contracts 1 volatility .4 fillCost 100
    val put = Option put 90 expiration 30 contracts 1 volatility .4 fillCost 100

    val spread : Spread = Spread short 2 of call long 2 of put time 5

    Stats of spread

    // Prints the PL at the given step interval
    PL of (spread step 1)

    println()

    // Stats of the spread 15 days later (not at time 15)
    Stats of (spread time 15)
    PL of (spread time 15)

    println()

    // Modify volatility of all options within the spread
    Stats of (spread volatility 0.2)
    PL of (spread volatility 0.2)

    println()

    // Modify the risk-free interest rate of all options within the spread
    Stats of (spread irate 0.05)
    PL of (spread irate 0.05)

    println()


    val spread2 = Spread long 1 of call long 1 of put

    Stats of (spread2 time 10 volatility 0.2)
    PL of (spread2 volatility 0.5)

    // Plot PL over time
    Plot of spread2 yAxis "PL" xAxis "time" show()

    // Plot Value over time
    Plot of spread2 yAxis "Value" xAxis "time" show()

    // Plot PL vs underlying
    Plot of spread2 yAxis "PL" xAxis "underlying" show()

  }
}