/**
  * Created by nicos on 4/17/17.
  */
object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val x = Option put "NVDA" strike 95.5 expiration 10 contracts 2
    print(x.ticker)
    print(x.strikePrice)

    // Is it possible to get:
    // Option.put().ticker("NVDA").strike(95.5).expiration(10)
  }
}