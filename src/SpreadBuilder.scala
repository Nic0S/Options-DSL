
class SpreadBuilder {
  var count : Integer = 0

  var spread : Spread = null

  def of (op : Option): Spread = {
    require(count != 0, "SpreadBuilder without count provided")

    if (spread == null) {
      op.costBasis *= count
      new Spread and (op contracts count)
    } else {
      op.costBasis *= count
      spread and (op contracts count)
    }
  }
}
