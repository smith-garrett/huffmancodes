import infra.*

@main
def main(s: String): Unit =
  val bin = encode(s)
  println(s"Code for $s: $bin")
