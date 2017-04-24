

object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val call = Option call 110 expiration 30 contracts 1 volatility .4 fillCost 100
    val put = Option put 90 expiration 30 contracts 1 volatility .4 fillCost 100

    val spread : Spread = Spread short 2 of call long 2 of put

    Stats of spread
    PL of spread

    println()

    Stats of (spread time 15 volatility .35)
    PL of (spread time 15 volatility .35)

    println()

    val spread2 : Spread = Spread long 1 of call long 1 of put

    Stats of spread2
    PL of spread2


  }
}