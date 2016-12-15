package lila.puzzle.tag

case class TagAggregateVote(up: Int, down: Int) {

  def sum = up - down

}