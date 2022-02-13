package org.clulab.dynet

import java.io.PrintWriter
import edu.cmu.dynet._
import org.clulab.dynet.ForwardLayer.TYPE_GREEDY
import edu.cmu.dynet.Expression.{concatenate, input, logSumExp, lookup, pick, pickNegLogSoftmax, sum}
import org.clulab.dynet.Utils.{ByLineFloatBuilder, ByLineIntBuilder, ByLineStringBuilder, ByLineStringMapBuilder, fromIndexToString, save}
import ForwardLayer._

import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.EdgeMap

import org.clulab.utils.MathUtils._
import org.clulab.struct.Edge
import org.clulab.utils
import java.util.Arrays

class GreedyForwardLayer (parameters:ParameterCollection,
                          inputSize: Int,
                          taskType: Int,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          rootParam: Parameter,
                          positionLookupParameters: LookupParameter, 
                          span: Option[Seq[(Int, Int)]],
                          nonlinearity: Int,
                          dropoutProb: Float)
  extends ForwardLayer(parameters, inputSize, taskType, t2i, i2t, H, rootParam, positionLookupParameters, span, nonlinearity, dropoutProb) {

  override def loss(finalStates: ExpressionVector, goldLabelStrings: IndexedSeq[String]): Expression = {
    val goldLabels = Utils.toIds(goldLabelStrings, t2i)
    Utils.sentenceLossGreedy(finalStates, goldLabels)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_GREEDY, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, taskType, "taskType")
    save(printWriter, span.map(spanToString).getOrElse("none"), "span")
    save(printWriter, nonlinearity, "nonlinearity")
    save(printWriter, t2i, "t2i")
    save(printWriter, dropoutProb, "dropoutProb")
  }

  override def toString: String = {
    s"GreedyForwardLayer($inDim, $outDim)"
  }

  override def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String] = {
    val labelIds = Utils.greedyPredict(emissionScores)
    labelIds.map(i2t(_))
  }

  override def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]] = {
    val labelsWithScores = new ArrayBuffer[IndexedSeq[(String, Float)]]

    for(scoresForPosition <- emissionScores) {
      val labelsAndScores = new ArrayBuffer[(String, Float)]()
      for(lid <- scoresForPosition.indices) {
        val label = i2t(lid)
        val score = scoresForPosition(lid)
        labelsAndScores += Tuple2(label, score)
      }
      labelsWithScores += labelsAndScores.sortBy(- _._2)
    }

    labelsWithScores
  }

  val YES_EDGE_ID = t2i(Utils.START_TAG)
  val NO_EDGE_ID = t2i(Utils.STOP_TAG)

  /**
   * Loss for the predicted graph
   * @param predictedGraph Graph predicted through inference
   * @param goldGraph The correct graph, containing only positive edges
   * @return Overall loss
   */
  override def graphLoss(emissionScoresAsExpression: Array[ExpressionVector], 
                         goldLabelStrings: IndexedSeq[String]): Expression = {
    assert(emissionScoresAsExpression.length == goldLabelStrings.length)
    val sum = new ExpressionVector()
    for(modifier <- emissionScoresAsExpression.indices) {
      val headRelativePosition = goldLabelStrings(modifier).toInt
      //val goldLabels = Utils.toIds(goldLabelStrings, t2i)
      //Utils.sentenceLossGreedy(emissionScoresAsExpression, goldLabels)

      // each array indicates with YES_EDGE_ID the position of the head word for the given modifier
      // we use the position of the modifier itself if its head is root; this allows the leantgh of goldLabels to be equal to that of the sentence
      val goldLabels = Array.fill[Int](emissionScoresAsExpression.length)(NO_EDGE_ID)
      goldLabels(modifier + headRelativePosition) = YES_EDGE_ID
      sum.add(Utils.sentenceLossGreedy(emissionScoresAsExpression(modifier), goldLabels))
    }
    Expression.sum(sum)

    /*
    //
    // loss = max(0, 1 + score(predictedGraph not in gold) - score(goldGraph not in pred) )
    //   where the score of a predicted edge not in gold is 1 + actual score in Expression
    //

    val lossParts = new ExpressionVector()
    lossParts.add(Expression.input(1.0f))

    val debug = false
    if(debug){
      println("Gold graph:")
      for(key <- goldGraph.keys.toList.sortBy(_._2)) {
        println("\t" + key)
      }
      println("Pred graph:")
      for(key <- predictedGraph.keys.toList.sortBy(_._2)) {
        println("\t" + key)
      }
    }

    // add the score of the predicted edges not in gold
    if(debug) println("predicted edges in the loss:")
    for(key <- predictedGraph.keys if ! goldGraph.contains(key)) {
      if(debug) println("\t" + key)
      val scores = predictedGraph(key)
      val scorePos = Expression.exprPlus(Expression.pick(scores, YES_EDGE_ID), 1f)
      //val scoreNeg = Expression.exprMinus(0f, Expression.pick(scores, NO_EDGE_ID))
      lossParts.add(scorePos)
      //lossParts.add(scoreNeg)
    }

    // subtract the score of gold edges not in pred
    if(debug) println("gold edges in the loss:")
    for(key <- goldGraph.keySet if ! predictedGraph.contains(key)) {
      if(debug) println("\t" + key)
      val scores = goldGraph(key)
      val scorePos = Expression.exprMinus(0f, Expression.pick(scores, YES_EDGE_ID))
      //val scoreNeg = Expression.pick(scores, NO_EDGE_ID)
      lossParts.add(scorePos)
      //lossParts.add(scoreNeg)
    }

    if(debug) utils.StringUtils.pressEnterKey()
    Expression.max(Expression.input(0f), Expression.sum(lossParts))
    */
  }

  /** Greedy method: for each modifier pick the head with the highest score */
  override def graphForward(inputExpressions: ExpressionVector, 
                            headPositionsOpt: Option[IndexedSeq[Int]],
                            doDropout: Boolean): Array[ExpressionVector] = {
    val emissionScores = new ArrayBuffer[ExpressionVector]
    val pH = Expression.parameter(H)
    val pRoot = Expression.parameter(rootParam)

    for(modifier <- inputExpressions.indices) {
      // these stores the scores for this modifier and each head in the sentence
      val scoresForModifier = new ExpressionVector()

      // fetch the relevant span from the RNN's hidden state
      var argExp = pickSpan(inputExpressions(modifier))
      // dropout on the hidden state
      val argExpAfterDropout = Utils.expressionDropout(argExp, dropoutProb, doDropout)

      for(head <- inputExpressions.indices) {
        val headExp = if(head == modifier) {
          // the setting head == modifier if reserved for root
          pRoot
        } else {
          pickSpan(inputExpressions(head))
        }
        val headExpAfterDropout = Utils.expressionDropout(headExp, dropoutProb, doDropout)

        val posEmbed = mkRelativePositionEmbedding(modifier, head)
        val ss = Expression.concatenate(argExpAfterDropout, headExpAfterDropout, posEmbed)
        var l1 = Utils.expressionDropout(pH * ss, dropoutProb, doDropout)
        l1 = applyNonlinearity(l1)

        scoresForModifier.add(l1)
      }

      emissionScores += scoresForModifier
    }

    emissionScores.toArray

    /*
    for (i <- inputExpressions.indices) {
        // fetch the relevant span from the RNN's hidden state
        var argExp = pickSpan(inputExpressions(i))
        // dropout on the hidden state
        argExp = Utils.expressionDropout(argExp, dropoutProb, doDropout)

        val predExp = Utils.expressionDropout(pRoot, dropoutProb, doDropout)
        val posEmbed = mkRelativePositionEmbedding(i, -1)

        val ss = Expression.concatenate(argExp, predExp, posEmbed)

        // forward layer + dropout on that
        var l1 = Utils.expressionDropout(pH * ss, dropoutProb, doDropout)

        // the last nonlinearity
        l1 = applyNonlinearity(l1)

        emissionScores.add(l1)
      }
    emissionScores
    */
                              
    /*                              
    //
    // the predicted graph
    //
    val predGraph = new EdgeMap[Expression]  

    for(modifier <- inputExpressions.indices) {
      var bestHead = -100
      var bestScore = scala.Float.MinValue
      var bestExpression:Option[Expression] = None

      // greedy algo: keep the head with the highest score
      for(head <- -1 until inputExpressions.size if head != modifier) {
        val scores = runForwardDual(modifier, head, inputExpressions, doDropout)
        val score = Expression.pick(scores, YES_EDGE_ID).value().toFloat()
        if(score > bestScore || bestHead < -1) {
          bestHead = head
          bestScore = score
          bestExpression = Some(scores)
        }
      }

      predGraph += ((bestHead, modifier), bestExpression.get)
    }

    //
    // the gold graph, using the heads from headPositionsOpt
    //
    val goldGraph = if (headPositionsOpt.nonEmpty) {
      val goldEdges = new EdgeMap[Expression]
      for(modifier <- inputExpressions.indices) {
        val head = headPositionsOpt.get(modifier)        
        val scores = 
          if(predGraph.contains((head, modifier))) {
            // reuse the same Expression from the predicted graph if shared edge
            predGraph(head, modifier)
          } else {
            runForwardDual(modifier, head, inputExpressions, doDropout)
          }
        goldEdges += ((head, modifier), scores)
      }
      Some(goldEdges)
    } else {
      None
    }

    (predGraph, goldGraph)
    */
  }

  /** Create the graph with Strings for labels */
  override def graphInference(emissionScores: Array[ExpressionVector]): IndexedSeq[String] = {
    // the prediction for each token is the relative position of the head; 0 means root
    val predictions = new ArrayBuffer[String]
    for(modifier <- emissionScores.indices) {
      val scoresForModifier = emissionScores(modifier)
      var bestHead = -1
      var bestScore = Float.MinValue
      for(head <- scoresForModifier.indices) {
        val scores =  scoresForModifier(head).value().toVector().toArray
        if(scores(YES_EDGE_ID) > bestScore) {
          bestScore = scores(YES_EDGE_ID)
          bestHead = head
        }
      }
      assert(bestHead >= 0)
      val relPos = bestHead - modifier
      predictions += relPos.toString()
    }
    predictions
    /*
    val predGraph = new EdgeMap[String]
    for(edge <- emissionScores.keys) {
      predGraph += (edge, Utils.START_TAG) // START_TAG indicates the presence of an (unlabeled) edge
    }
    predGraph
    */

  }
}

object GreedyForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): GreedyForwardLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineFloatBuilder = new ByLineFloatBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()
    val byLineStringBuilder = new ByLineStringBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator, "inputSize")
    val taskType = byLineIntBuilder.build(x2iIterator, "taskType", ForwardLayer.DEFAULT_BASIC)
    val spanValue = byLineStringBuilder.build(x2iIterator, "span", "")
    val span = if(spanValue.isEmpty || spanValue == "none") None else Some(parseSpan(spanValue, inputSize))
    val nonlinearity = byLineIntBuilder.build(x2iIterator, "nonlinearity", ForwardLayer.NONLIN_NONE)
    val t2i = byLineStringMapBuilder.build(x2iIterator, "t2i")
    val i2t = fromIndexToString(t2i)
    val dropoutProb = byLineFloatBuilder.build(x2iIterator, "dropoutProb", ForwardLayer.DEFAULT_DROPOUT_PROB)

    //
    // make the loadable parameters
    //
    //println(s"making FF ${t2i.size} x ${2 * inputSize}")
    val needsDoubleLength = ! TaskManager.isBasic(taskType)
    val actualInputSize =
      if(span.nonEmpty) {
        val len = ForwardLayer.spanLength(span.get)
        if(needsDoubleLength) 2 * len else len
      } else {
        if(needsDoubleLength) (2 * inputSize + 32) else inputSize
      }

    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))
    val positionLookupParameters = parameters.addLookupParameters(101, Dim(32))

    new GreedyForwardLayer(parameters,
      inputSize, taskType, t2i, i2t, H, rootParam, positionLookupParameters, 
      span, nonlinearity, dropoutProb)
  }
}


