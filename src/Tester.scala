

object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val call = Option call "NVDA" strike 110 expiration 30 contracts 1 volatility .4 fillCost 100
    val put = Option put "NVDA" strike 90 expiration 30 contracts 1 volatility .4 fillCost 100


    val spread = Spread of call and put

    
  }
}