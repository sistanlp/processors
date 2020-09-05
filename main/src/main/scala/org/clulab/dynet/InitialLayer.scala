package org.clulab.dynet

import edu.cmu.dynet.ExpressionVector

/**
 * First layer that occurs in a sequence modeling architecture: goes from words to Expressions
 */
trait InitialLayer extends Saveable {
  def forward(sentence: AnnotatedSentence,
              doDropout: Boolean, taskId: Option[Int]): ExpressionVector

  def outDim:Int // output dimension
}
