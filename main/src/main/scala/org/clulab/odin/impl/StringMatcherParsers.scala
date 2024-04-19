package org.clulab.odin.impl

import scala.util.matching.Regex
import scala.util.parsing.combinator._

trait StringMatcherParsers extends RegexParsers {

  // any valid StringMatcher
  def stringMatcher: Parser[StringMatcher] = exactStringMatcher | regexStringMatcher

  // a StringMatcher that compares to a string
  def exactStringMatcher: Parser[ExactStringMatcher] = stringLiteral ^^ {
    string => new ExactStringMatcher(string)
  }

  // a StringMatcher that uses a regex
  def regexStringMatcher: Parser[RegexStringMatcher] = regexLiteral ^^ {
    regex => new RegexStringMatcher(regex)
  }

  // any valid string literal (with or without quotes)
  def stringLiteral: Parser[String] = odinIdentifier | quotedStringLiteral

  // valid java identifier
  def javaIdentifier: Parser[String] =
    """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  // identifier that can also contain colons and dashes except in the first and last characters
  def odinIdentifier: Parser[String] =
    """\p{javaJavaIdentifierStart}(?:[-:\p{javaJavaIdentifierPart}]*\p{javaJavaIdentifierPart})?""".r

  // single- or double-quote delimited string literal
  // with "\" as the escape character
  def quotedStringLiteral: Parser[String] =
    """'[^\\']*(?:\\.[^\\']*)*'|"[^\\"]*(?:\\.[^\\"]*)*"""".r ^^ {
      s => """\\(.)""".r.replaceAllIn(s.drop(1).dropRight(1), m => m.group(1))
    }

  // match a perl style "/" delimited regular expression
  // "\" is the escape character, so "\/" becomes "/"
  def regexLiteral: Parser[Regex] = """/[^\\/]*(?:\\.[^\\/]*)*/""".r ^^ {
    s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/").r
  }

}

sealed trait StringMatcher {
  def matches(s: String): Boolean

}

class ExactStringMatcher(string: String) extends StringMatcher {
  def matches(s: String): Boolean = string == s

  override def toString: String = s"$string"
}

class RegexStringMatcher(regex: Regex) extends StringMatcher {
  def matches(s: String): Boolean = regex.findFirstIn(s).nonEmpty

  override def toString: String = s"the regex to match: $regex"
}
