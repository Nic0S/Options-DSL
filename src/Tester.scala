

object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val call = Option call "NVDA" strike 110 expiration 30 contracts 1 volatility .4 fillCost 100
    val put = Option put "NVDA" strike 90 expiration 30 contracts 1 volatility .4 fillCost 100


    val spread = Spread of call and put

    println(spread)

    println(spread calculatePL 100)
    println(spread calculatePL 80)
    println(spread calculatePL 120)

    println(spread expiration 1 calculatePL 100)
    println(spread expiration 1 calculatePL 80)
    println(spread expiration 1 calculatePL 120)

    spread expiration 15 start 80 end 120 step 5 print "PL"
  }
}