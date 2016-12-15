package lila.puzzle.tag

case class TagVoted(tag: Tag, vote: TagAggregateVote) {

  def trusted: Boolean = vote.sum > 2

}

case class TagVoteds(value: List[TagVoted])