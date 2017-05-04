object CommonStrategies extends OptionsDSL {
  def main ( args: Array[String]) : Unit = {

    // Straddle
//    var call = Option call 100 expiration 60 volatility 0.35 fillCost 100
//    var put = Option put 100 expiration 60 volatility 0.35 fillCost 100
//
//    var spread = Spread short 1 of call short 1 of put
//
//    Plot of (spread time 60) min 85 max 115 show()

    // Butterfly
//    var high_call = Option call 110 expiration 60 volatility 0.35 fillCost 100
//    var mid_call = Option call 100 expiration 60 volatility 0.35 fillCost 100
//    var low_call = Option call 90 expiration 60 volatility 0.35 fillCost 100
//
//    var butterfly = Spread short 2 of mid_call long 1 of high_call long 1 of low_call
//
//    Plot of (butterfly time 60) min 80 max 120 show()


    // Broken Wing Butterfly
    var high_call = Option call 120 expiration 60 volatility 0.35 fillCost 100
    var mid_call = Option call 105 expiration 60 volatility 0.35 fillCost 100
    var low_call = Option call 100 expiration 60 volatility 0.35 fillCost 100

    var bw_butterfly = Spread long 1 of high_call short 2 of mid_call long 1 of low_call

    Plot of (bw_butterfly time 60) min 80 max 120 show()
  }
}
