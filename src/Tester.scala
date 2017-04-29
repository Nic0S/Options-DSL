
object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val call = Option call 110 expiration 30 contracts 1 volatility .4 fillCost 100
    val put = Option put 90 expiration 30 contracts 1 volatility .4 fillCost 100

    val spread : Spread = Spread short 2 of call long 2 of put time 5

    Stats of spread
    PL of (spread step 1)

    println()

    Stats of (spread time 15 volatility .35)
    PL of (spread time 15 volatility .35)

    println()


    // fix step at end of this line
    val spread2 = Spread long 1 of call long 1 of put

    Stats of (spread2 time 10 volatility 0.2)
    PL of (spread2 volatility 0.5)

    Plot of spread2 yAxis "PL" xAxis "time" show()

  }
}