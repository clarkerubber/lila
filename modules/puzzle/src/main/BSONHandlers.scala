package lila.puzzle.tag

import org.joda.time.DateTime
import reactivemongo.bson._

import lila.db.BSON
import lila.db.BSON.{ Reader, Writer }
import lila.db.dsl._

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
}
