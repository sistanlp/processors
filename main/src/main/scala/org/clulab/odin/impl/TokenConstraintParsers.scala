package org.clulab.odin.impl
import org.clulab.processors.Document
import org.clulab.odin._


trait TokenConstraintParsers extends StringMatcherParsers {

  def tokenConstraint: Parser[TokenConstraint] =
    "[" ~> opt(disjunctiveConstraint) <~ "]" ^^ {
      case Some(constraint) => constraint
      case None => TokenWildcard
    }

  def config: OdinConfig

  /** access to resources (w2v embeddings, wordnet, etc) */
  val resources = config.resources

  /** the field that should be used by unitConstraint */
  def unit: String

  /** makes a token constraint from a single string matcher */
  def unitConstraint: Parser[TokenConstraint] = stringMatcher ^^ {
    case matcher => unit match {
      case "word" => new WordConstraint(matcher)
      case "tag" => new TagConstraint(matcher)
      case _ => sys.error("unrecognized token field")
    }
  }

  def disjunctiveConstraint: Parser[TokenConstraint] =
    rep1sep(conjunctiveConstraint, "|") ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) => new DisjunctiveConstraint(lhs, rhs)
      }
    }

  def conjunctiveConstraint: Parser[TokenConstraint] =
    rep1sep(negatedConstraint, "&") ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) => new ConjunctiveConstraint(lhs, rhs)
      }
    }

  def negatedConstraint: Parser[TokenConstraint] = opt("!") ~ atomicConstraint ^^ {
    case None ~ constraint => constraint
    case Some("!") ~ constraint => new NegatedConstraint(constraint)
    case _ => sys.error("unrecognized negatedConstraint operator")
  }

  def atomicConstraint: Parser[TokenConstraint] =
    numericConstraint | fieldConstraint | "(" ~> disjunctiveConstraint <~ ")"

  def fieldConstraint: Parser[TokenConstraint] = javaIdentifier ~ "=" ~ stringMatcher ~ opt("." ~> javaIdentifier) ^^ {
    case "word"     ~ "=" ~ matcher ~ None => new WordConstraint(matcher)
    case "lemma"    ~ "=" ~ matcher ~ None => new LemmaConstraint(matcher)
    case "tag"      ~ "=" ~ matcher ~ None => new TagConstraint(matcher)
    case "entity"   ~ "=" ~ matcher ~ None => new EntityConstraint(matcher)
    case "chunk"    ~ "=" ~ matcher ~ None => new ChunkConstraint(matcher)
    case "norm"     ~ "=" ~ matcher ~ None => new NormConstraint(matcher)
    case "incoming" ~ "=" ~ matcher ~ None => new IncomingConstraint(matcher, config.graph)
    case "outgoing" ~ "=" ~ matcher ~ None => new OutgoingConstraint(matcher, config.graph)
    case "mention"  ~ "=" ~ matcher ~ arg  => new MentionConstraint(matcher, arg)
    case _ => sys.error("unrecognized token field")
  }

  /** for numerical comparisons */
  def numericExpression: Parser[NumericExpression] =
    productExpression ~ rep(("+" | "-") ~ productExpression) ^^ {
      case prod ~ list => list.foldLeft(prod) {
        case (lhs, "+" ~ rhs) => new Addition(lhs, rhs)
        case (lhs, "-" ~ rhs) => new Subtraction(lhs, rhs)
        case _ => sys.error("unrecognized numericExpression operator")
      }
    }

  def productExpression: Parser[NumericExpression] =
    termExpression ~ rep(("*" | "//" | "/" | "%") ~ termExpression) ^^ {
      case prod ~ list => list.foldLeft(prod) {
        case (lhs, "*" ~ rhs) => new Multiplication(lhs, rhs)
        case (lhs, "/" ~ rhs) => new Division(lhs, rhs)
        case (lhs, "//" ~ rhs) => new EuclideanQuotient(lhs, rhs)
        case (lhs, "%" ~ rhs) => new EuclideanRemainder(lhs, rhs)
        case _ => sys.error("unrecognized productExpression operator")
      }
    }

  def termExpression: Parser[NumericExpression] = opt("-" | "+") ~ atomicExpression ^^ {
    case None ~ expr => expr
    case Some("-") ~ expr => new NegativeExpression(expr)
    case Some("+") ~ expr => expr
    case _ => sys.error("unrecognized termExpression operator")
  }

  def atomicExpression: Parser[NumericExpression] =
    numberConstant | numericFunction | "(" ~> numericExpression <~ ")"

  // an equality/inequality symbol
  // the longest strings must come first
  def compareOps: Parser[String] = ">=" | "<=" | "==" | "!=" | ">" | "<"

  def numberConstant: Parser[NumericExpression] = numberLiteral ^^ { new Constant(_) }

  def numberLiteral: Parser[Double] =
    """(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][-+]?\d+)?""".r ^^ { _.toDouble }

  /** update as needed.  Currently only a distributional similarity comparison */
  def numericFunction: Parser[NumericExpression] = simScore

  def numericConstraint: Parser[TokenConstraint] =
    numericExpression ~ compareOps ~ numericExpression ~ rep(compareOps ~ numericExpression) ^^ {
      case lhs ~ op ~ rhs ~ rest =>
        var prev = rhs
        val first = mkCompare(lhs, op, rhs)
        rest.foldLeft(first) {
          case (l, op ~ expr) =>
            val r = mkCompare(prev, op, expr)
            prev = expr
            new ConjunctiveConstraint(l, r)
        }
    }

  def mkCompare(lhs: NumericExpression, op: String, rhs: NumericExpression): TokenConstraint =
    op match {
      case ">" => new GreaterThan(lhs, rhs)
      case "<" => new LessThan(lhs, rhs)
      case ">=" => new GreaterThanOrEqual(lhs, rhs)
      case "<=" => new LessThanOrEqual(lhs, rhs)
      case "==" => new Equal(lhs, rhs)
      case "!=" => new NotEqual(lhs, rhs)
    }

  /**
   * ex. get the distributional similarity score (dot product of L2 normed vectors)
   * for a specified word and the current token
   * */
  def simScore: Parser[NumericExpression] = "simScore" ~> "(" ~> stringLiteral <~ ")" ^^ {
    case w2 if resources.embeddings.nonEmpty => new SimilarityConstraint(w2, resources.embeddings.get)
    case missing if resources.embeddings.isEmpty =>
      throw new OdinCompileException("Error compiling pattern using 'simTo'. No embeddings specified in 'resources'")
  }

}




/** for numerical comparisons */
sealed trait NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double
}

class Constant(val value: Double) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double = value
}

class Addition(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) + rhs.number(tok, sent, doc, state)
}

class Subtraction(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) - rhs.number(tok, sent, doc, state)
}

class Multiplication(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) * rhs.number(tok, sent, doc, state)
}

class Division(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) / rhs.number(tok, sent, doc, state)
}

class EuclideanQuotient(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double = {
    val a = lhs.number(tok, sent, doc, state)
    val n = rhs.number(tok, sent, doc, state)
    math.signum(n) * math.floor(a / math.abs(n))
  }
}

class EuclideanRemainder(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double = {
    val a = lhs.number(tok, sent, doc, state)
    val n = math.abs(rhs.number(tok, sent, doc, state))
    a - n * math.floor(a / n)
  }
}

class NegativeExpression(expr: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    -expr.number(tok, sent, doc, state)
}

/** matcher must be an exact string matcher, so that a particular word vector can be retrieved */
class SimilarityConstraint(w1: String, embeddings: EmbeddingsResource) extends NumericExpression with Values {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double = {
    // current word
    val w2 = word(tok, sent, doc)

    // no need to check to see if w1 & w2 in embeddings
    // this is handled by the EmbeddingsResource
    embeddings.similarity(w1, w2)
  }
}

class GreaterThan(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) > rhs.number(tok, sent, doc, state)
}

class LessThan(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) < rhs.number(tok, sent, doc, state)
}

class GreaterThanOrEqual(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) >= rhs.number(tok, sent, doc, state)
}

class LessThanOrEqual(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) <= rhs.number(tok, sent, doc, state)
}

class Equal(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) == rhs.number(tok, sent, doc, state)
}

class NotEqual(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) != rhs.number(tok, sent, doc, state)
}

/** matches any token, provided that it exists */
object TokenWildcard extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    doc.sentences(sent).words.isDefinedAt(tok)
}

class WordConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches word(tok, sent, doc)

  override def toString: String = s"$matcher"
}

class LemmaConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches lemma(tok, sent, doc)

  override def toString: String = s"$matcher"
}

class TagConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches tag(tok, sent, doc)

  override def toString: String = s"$matcher"
}

class EntityConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches entity(tok, sent, doc)

  override def toString: String = s"$matcher"

}

class ChunkConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches chunk(tok, sent, doc)
}

class NormConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches norm(tok, sent, doc)
}

class IncomingConstraint(matcher: StringMatcher, graphName: String) extends TokenConstraint with Graph {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    incoming(tok, sent, doc, graphName) exists matcher.matches
}

class OutgoingConstraint(matcher: StringMatcher, graphName: String) extends TokenConstraint with Graph {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    outgoing(tok, sent, doc, graphName) exists matcher.matches
}

// checks that a token is inside a mention
class MentionConstraint(matcher: StringMatcher, arg: Option[String]) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean = {
    val mentions = state.mentionsFor(sent, tok).filter(_ matches matcher)
    val results = arg match {
      case None => mentions
      case Some(name) if name equalsIgnoreCase "trigger" => mentions.flatMap {
        case e: EventMention => Some(e.trigger)
        case _ => None
      }
      case Some(name) => mentions.flatMap(_.arguments.getOrElse(name, Nil))
    }
    // we only need to know if there is at least one mention that matches
    results.exists(_.tokenInterval contains tok)
  }
}

class NegatedConstraint(constraint: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    !constraint.matches(tok, sent, doc, state)
}

class ConjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.matches(tok, sent, doc, state) && rhs.matches(tok, sent, doc, state)

  override def toString: String = s"lhs is $lhs and rhs is $rhs"

}

class DisjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.matches(tok, sent, doc, state) || rhs.matches(tok, sent, doc, state)
}

sealed trait TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean
}