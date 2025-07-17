// === Gensym Utility ===
object Gensym:
  private var counter = 0
  def fresh(prefix: String = "_"): String =
    counter += 1
    s"$prefix$counter"
