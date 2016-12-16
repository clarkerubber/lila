package lila.puzzle.tag

case class TagAggregateVote(up: Int, down: Int) {

  def sum = up - down

  def change(from: Boolean, to: Boolean) = if (from == to) this else copy(
    up = up + (if (to) 1 else -1),
    down = down + (if (to) -1 else 1))

  def add(v: Boolean) = copy(
    up = up + (if (v) 1 else 0),
    down = down + (if (v) 0 else 1))

}

object TagAggregateVote {

  def make(up: Int = 0, down: Int = 0) = TagAggregateVote(up, down)
}