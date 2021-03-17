package org.clulab.embeddings

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.InputStreamer
import org.clulab.utils.InputStreamer.StreamResult

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

/** Manages a pool of word embedding maps, so we do not load them more than once */
object WordEmbeddingMapPool {
  import scala.concurrent.ExecutionContext.Implicits.global

  case class Key(name: String, compact: Boolean)

  protected val inputStreamer = new InputStreamer()
  protected val maxWaitTime = Duration.Inf

  /** Stores all embedding maps that have been accessed */
  protected val pool = new mutable.HashMap[Key, Future[WordEmbeddingMap]]()

  /** Fetches an embedding from the pool if it exists, or creates it otherwise */
  def getOrElseCreate(name: String, compact: Boolean = false, fileLocation: String = "", resourceLocation: String = ""): WordEmbeddingMap = {
    val wordEmbeddingMapFuture = this.synchronized {
      // Access the shared pool inside the synchronized section.
      pool.getOrElseUpdate(
        Key(name, compact),
        Future {
          loadEmbedding(name, fileLocation, resourceLocation, compact = compact)
        }
      )
    }
    // Wait for the result outside the synchronized section.
    Await.result(wordEmbeddingMapFuture, maxWaitTime)
  }

  /** Removes an embedding map from the pool */
  def remove(filename: String, compact: Boolean = false): Unit = {
    this.synchronized {
      pool.remove(Key(filename, compact))
    }
  }

  def clear(): Unit = {
    this.synchronized {
      pool.clear()
    }
  }

  protected def loadEmbedding(name: String, fileLocation: String, resourceLocation: String, compact: Boolean): WordEmbeddingMap = {
    val StreamResult(inputStream, _, format) = inputStreamer.stream(name, fileLocation, resourceLocation)
        .getOrElse(throw new RuntimeException(s"WordEmbeddingMap $name could not be opened."))
    val wordEmbeddingMap = inputStream.autoClose { inputStream =>
      val binary = format == InputStreamer.Format.Bin

      if (compact) CompactWordEmbeddingMap(inputStream, binary)
      else ExplicitWordEmbeddingMap(inputStream, binary)
    }

    wordEmbeddingMap
  }
}