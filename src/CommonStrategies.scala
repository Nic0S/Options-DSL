object CommonStrategies extends OptionsDSL {
  def main ( args: Array[String]) : Unit = {

    // Straddle
    var call = Option call 100 expiration 60 volatility 0.35 fillCost 100
    var put = Option put 100 expiration 60 volatility 0.35 fillCost 100

    var spread = Spread short 1 of call short 1 of put

    Plot of (spread time 60) min 85 max 115 show()
  }
}
