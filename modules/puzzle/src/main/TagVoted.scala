package lila.puzzle

case class TagVoted(tag: Tag, vote: TagAggregateVote) {

  def trusted: Boolean = vote.sum > 3

}

case class TagVoteds(value: List[TagVoted])
