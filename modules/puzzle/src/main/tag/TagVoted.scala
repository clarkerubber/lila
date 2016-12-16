package lila.puzzle.tag

case class TagVoted(tag: Tag, vote: TagAggregateVote) {

  def trusted: Boolean = vote.sum > 2

}

case class TagVoteds(value: List[TagVoted]) {

  def without(tag: Tag): List[TagVoted] = value.filterNot(_.tag == tag)

  def change(tag: Tag, from: Option[TagVote], to: Boolean): TagVoteds = value.find(_.tag == tag) match {
    case Some(t) => from match {
      case Some(f) => if (f.value == to) this else copy(value = TagVoted(tag, t.vote.change(f.value, to)) :: this.without(tag))
      case _ => copy(value = TagVoted(tag, t.vote.add(to)) :: this.without(tag))
    }
    case _ => copy(value = TagVoted(tag, TagAggregateVote.make().add(to)) :: value)
  }
}