package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  // abstract method for isEmpty
  def isEmpty: Boolean

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def union(that: TweetSet): TweetSet // this was left to be an abstract method and implemented in the subclasses
  
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */

  // this method is left abstract and is implemented in the subclasses since it'll be handled differently for each class
    def mostRetweeted: Tweet
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */

    // this method is left abstract and is implemented in the subclasses
    def descendingByRetweet: TweetList
  
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  // defining the union in this class because it's left to be abstract in the TweetSet
  // a union of an Empty set will return the sent in TweetSet
  def union(that: TweetSet): TweetSet = that

  // initialize isEmpty
  def isEmpty: Boolean = true

  // this method is not needed since the decending tweets is null for an empty set
  /*
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet = throw new NoSuchElementException

  // defining the descendingByRetweet in this class because it's left to be abstract in the TweetSet
  // nothing will be returned if the set is empty
  def descendingByRetweet: TweetList = Nil

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  // the filterAcc should return if the element is a tweet then include the it in the correct set either left of right
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if(p(elem)) {
      left.filterAcc(p, right.filterAcc(p, acc.incl(elem)))
    } else {
      left.filterAcc(p, right.filterAcc(p, acc))
    }
  }

  // defining the union in this class because it's left to be abstract in the TweetSet
  // a union of a NonEmpty set will return the filterAcc of the tweet
  def union(that: TweetSet): TweetSet = this.filterAcc(t => true, that)

  // defined for mostRetweeted
  def isEmpty: Boolean = false

  // define for mostRetweeted
  def max(t1: Tweet, t2: Tweet): Tweet =
    if (t1.retweets > t2.retweets) t1 else t2


  // define mostRetweeted so decending by retweet can order the list correctly
  // this method is not needed for empty sets
  def mostRetweeted: Tweet = {
    // if the lists are empty then return the element
    if(left.isEmpty && right.isEmpty) elem
    else if (left.isEmpty) max(elem, right.mostRetweeted)
    else if (right.isEmpty) max(elem, left.mostRetweeted)
    else {
      // check if current is max
      def maxRetweeeted = max(left.mostRetweeted, right.mostRetweeted)
      max(maxRetweeeted, elem)
    }
  }

  // defining the descendingByRetweet in this class because it's left to be abstract in the TweetSet
  // nothing will be returned if the set is empty
  // hint to take advantage of remove to help order the list
  def descendingByRetweet: TweetList = {
    val max = mostRetweeted
    // continue to order the max to the head and process the rest of the list
    new Cons(max, remove(max).descendingByRetweet)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = ???
  lazy val appleTweets: TweetSet = ???
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
     lazy val trending: TweetList = ???
  }

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
