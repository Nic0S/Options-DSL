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

  override def toString() : String = {
    var s : String = ""

    for (o : Option <- options) {
      s ++= o.toString() + "\n"
    }

    s
  }
}
