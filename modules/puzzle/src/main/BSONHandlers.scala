package lila.puzzle

import org.joda.time.DateTime
import reactivemongo.bson._

import lila.db.BSON
import lila.db.BSON.{ Reader, Writer }
import lila.db.dsl._
import lila.puzzle.tag._

private object BSONHandlers {

  implicit val tagAggregateVoteBSONHandler = reactivemongo.bson.Macros.handler[TagAggregateVote]

  // see exemple in Puzzle.scala line 123 (should be moved in here)

  implicit val tagsBSONHandler = new BSON[TagVoteds] {

    def reads(r: BSON.Reader): TagVoteds = TagVoteds(r.toList map {
      case BSONElement(id, v: BSONDocument) => (Tag.byId(id), tagAggregateVoteBSONHandler.read(v)) match {
        case (Some(t), k) => TagVoted(t, k)
        case _ => throw new Exception(s"Cannot convert $id to Tag")
      }
      case _ => throw new Exception("malformed BSONDocument")
    })

    def writes(w: BSON.Writer, o: TagVoteds): Bdoc = $doc(o.value map{
      case TagVoted(tag, tav) => BSONElement(tag.id, tagAggregateVoteBSONHandler.write(tav))
    })
  }

  import reactivemongo.bson._
  import lila.db.BSON
  import BSON.BSONJodaDateTimeHandler
  import lila.puzzle._
  private implicit val lineBSONHandler = new BSONHandler[BSONDocument, Lines] {
    private def readMove(move: String) = chess.Pos.doublePiotrToKey(move take 2) match {
      case Some(m) => s"$m${move drop 2}"
      case _       => sys error s"Invalid piotr move notation: $move"
    }
    def read(doc: BSONDocument): Lines = doc.elements.toList map {
      case BSONElement(move, BSONBoolean(true))  => Win(readMove(move))

      case BSONElement(move, BSONBoolean(false)) => Retry(readMove(move))

      case BSONElement(move, more: BSONDocument) =>
        Node(readMove(move), read(more))

      case BSONElement(move, value) =>
        throw new Exception(s"Can't read value of $move: $value")
    }
    private def writeMove(move: String) = chess.Pos.doubleKeyToPiotr(move take 4) match {
      case Some(m) => s"$m${move drop 4}"
      case _       => sys error s"Invalid move notation: $move"
    }
    def write(lines: Lines): BSONDocument = BSONDocument(lines map {
      case Win(move)         => writeMove(move) -> BSONBoolean(true)
      case Retry(move)       => writeMove(move) -> BSONBoolean(false)
      case Node(move, lines) => writeMove(move) -> write(lines)
    })
  }

  implicit val puzzleBSONHandler = new BSON[Puzzle] {

    import lila.puzzle.Puzzle.BSONFields._
    import PuzzlePerf.puzzlePerfBSONHandler
    import AggregateVote.aggregatevoteBSONHandler
    import chess.Color

    def reads(r: BSON.Reader): Puzzle = Puzzle(
      id = r int id,
      gameId = r str gameId,
      history = r str history split ' ' toList,
      fen = r str fen,
      lines = r.get[Lines](lines),
      depth = r int depth,
      color = Color(r bool white),
      date = r date date,
      perf = r.get[PuzzlePerf](perf),
      vote = r.get[AggregateVote](vote),
      attempts = r int attempts,
      mate = r bool mate,
      tags = r.get[TagVoteds](tags))

    def writes(w: BSON.Writer, o: Puzzle) = BSONDocument(
      id -> o.id,
      gameId -> o.gameId,
      history -> o.history.mkString(" "),
      fen -> o.fen,
      lines -> o.lines,
      depth -> o.depth,
      white -> o.color.white,
      date -> o.date,
      perf -> o.perf,
      vote -> o.vote,
      attempts -> o.attempts,
      mate -> o.mate,
      tags -> o.tags)
  }
}
