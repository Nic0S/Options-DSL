/**
  * Created by nicos on 4/17/17.
  */
object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val x = Option call "NVDA" strike 100 expiration 10 contracts -2 volatility .25
    print(x calculatePrice 120)


  }
}