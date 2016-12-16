package lila.puzzle

import chess.Color
import chess.format.{ Uci, Forsyth }
import org.joda.time.DateTime
import lila.puzzle.tag._

case class Puzzle(
    id: PuzzleId,
    gameId: String,
    history: List[String],
    fen: String,
    lines: List[Line],
    depth: Int,
    color: Color,
    date: DateTime,
    perf: PuzzlePerf,
    vote: AggregateVote,
    attempts: Int,
    mate: Boolean,
    tags: TagVoteds) {

  // ply after "initial move" when we start solving
  def initialPly: Int = {
    fen.split(' ').lastOption flatMap parseIntOption map { move =>
      move * 2 - color.fold(0, 1)
    }
  } | 0

  def enabled = vote.ratio > AggregateVote.minRatio || vote.nb < AggregateVote.minVotes

  def withVote(f: AggregateVote => AggregateVote) = copy(vote = f(vote))

  def initialMove: Uci.Move = history.lastOption flatMap Uci.Move.apply err s"Bad initial move $this"

  def fenAfterInitialMove: Option[String] = {
    for {
      sit1 <- Forsyth << fen
      sit2 <- sit1.move(initialMove).toOption.map(_.situationAfter)
    } yield Forsyth >> sit2
  }

  def withTagVote(tag: Tag, from: Option[Boolean], to: Boolean) = copy(tags = tags.change(tag, from, to))

  def trustedTags: List[TagVoted] = tags.value.filter(_.trusted)

  def trustedTagIds: List[String] = trustedTags map(_.tag.id)
}

object Puzzle {

  def make(
    gameId: String,
    history: List[String],
    fen: String,
    color: Color,
    lines: Lines,
    mate: Boolean)(id: PuzzleId) = new Puzzle(
    id = id,
    gameId = gameId,
    history = history,
    fen = fen,
    lines = lines,
    depth = Line minDepth lines,
    color = color,
    date = DateTime.now,
    perf = PuzzlePerf.default,
    vote = AggregateVote.default,
    attempts = 0,
    mate = mate,
    tags = TagVoteds(Nil))

  object BSONFields {
    val id = "_id"
    val gameId = "gameId"
    val history = "history"
    val fen = "fen"
    val lines = "lines"
    val depth = "depth"
    val white = "white"
    val date = "date"
    val perf = "perf"
    val rating = s"$perf.gl.r"
    val vote = "vote"
    val voteNb = s"$vote.nb"
    val voteRatio = s"$vote.ratio"
    val day = "day"
    val attempts = "attempts"
    val mate = "mate"
    val tags = "tags"
    val trustedTags = "trustedTags"
  }
}
