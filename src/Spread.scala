import java.util

class Spread {
  var options = List[Option]()

  def and (op : Option) : Spread = {
    val copy : Spread = this.copy()
    copy.options = copy.options :+ op
    copy
  }

  def copy() : Spread = {
    val copy = new Spread
    for (o : Option <- options) {
      copy.options = copy.options :+ (o copy())
    }
    copy
  }

  def calculatePrice (underlyingPrice : Double) : Double = {
    var totalPrice : Double = 0

    for (o : Option <- options) {
      totalPrice += o calculatePrice underlyingPrice
    }

    totalPrice
  }

  def calculatePL (underlyingPrice : Double) : Double = {
    var totalPL : Double = 0

    for (o : Option <- options) {
      totalPL += o calculatePL underlyingPrice
    }

    totalPL
  }

  override def toString() : String = {
    var s : String = ""

    for (o : Option <- options) {
      s ++= o.toString() + "\n"
    }

    s
  }
}
