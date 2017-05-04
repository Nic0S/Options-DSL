
// This class represents an intermediate spread, where "long x" or "short x" has been defined, but the option that
// is bought or sold has not been defined yet. A SpreadBuilder must of followed by: of <Option> to do anything with it.

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
