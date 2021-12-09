package rpt.udemy
package section2.list

import scala.annotation.tailrec


trait LinkList[+T] {
  def head: T

  def tail: LinkList[T]

  def last: T

  def isEmpty: Boolean

  def :+[A >: T](element: A): LinkList[A]

  def prepend[A >: T](element: A): LinkList[A]

  def reverse: LinkList[T]

  def zip[A](linkList: LinkList[A]): LinkList[(T, A)]

  def zipWith[A, B](linkList: LinkList[A], f: ((T, A)) => B): LinkList[B]

  def sort(comparator: (T, T) => Int): LinkList[T]

  def forEach(f: T => Unit): Unit

  def map[A >: T](mapFunction: T => A): LinkList[A]

  def flatMap[A >: T](flatMapFunction: T => LinkList[A]): LinkList[A]

  def filter(predicate: T => Boolean): LinkList[T]

  def ++[A >: T](other: LinkList[A]): LinkList[A]

  def strRepresentation: String

  override def toString: String = s"[$strRepresentation]"
}

object LinkList {
  def apply[T](elements: T*): LinkList[T] = {
    @tailrec
    def helper(acc: LinkList[T], remaining: Seq[T]): LinkList[T] = {
      if (remaining.isEmpty) {
        acc
      } else {
        helper(acc :+ remaining.head, remaining.tail)
      }
    }

    helper(EmptyLink, elements)
  }
  def singleton[T](element:T):LinkList[T] = Link(element,EmptyLink)
}

class EmptyHeadException extends IllegalArgumentException("An Empty list has no head")

class EmptyLastException extends IllegalArgumentException("An Empty list has no last")

object EmptyLink extends LinkList[Nothing] {

  override def head: Nothing = throw new EmptyHeadException

  override def last: Nothing = throw new EmptyLastException

  override def tail: LinkList[Nothing] = EmptyLink

  override def isEmpty: Boolean = true

  override def :+[A >: Nothing](element: A): LinkList[A] = EmptyLink.prepend(element)

  override def prepend[A >: Nothing](element: A): LinkList[A] = LinkList.singleton(element)

  override def reverse: LinkList[Nothing] = EmptyLink

  override def sort(comparator: (Nothing, Nothing) => Int): LinkList[Nothing] = EmptyLink

  override def zip[A](linkList: LinkList[A]): LinkList[(Nothing, A)] = EmptyLink

  override def zipWith[A, B](linkList: LinkList[A], f: ((Nothing, A)) => B): LinkList[B] = EmptyLink

  override def forEach(f: Nothing => Unit): Unit = {}

  override def map[A](mapFunction: Nothing => A): LinkList[A] = EmptyLink

  override def flatMap[A](flatMapFunction: Nothing => LinkList[A]): LinkList[A] = EmptyLink

  override def filter(predicate: Nothing => Boolean): LinkList[Nothing] = EmptyLink

  override def ++[A >: Nothing](other: LinkList[A]): LinkList[A] = other

  override def strRepresentation: String = ""

}


case class Link[T](head: T, tail: LinkList[T]) extends LinkList[T] {

  override def isEmpty: Boolean = false

  override def last: T = {
    if (tail.isEmpty) {
      head
    } else {
      tail.last
    }
  }

  override def :+[A >: T](element: A): LinkList[A] = Link[A](head, tail :+ element)

  override def prepend[A >: T](element: A): LinkList[A] = Link[A](element, Link(head, tail))

  override def reverse: LinkList[T] = {
    if (tail.isEmpty) {
      this
    } else {
      @tailrec
      def helper(remaining: LinkList[T] = this, acc: LinkList[T] = EmptyLink): LinkList[T] = {
        if (remaining.isEmpty) {
          acc
        } else {
          helper(remaining.tail, acc.prepend(remaining.head))
        }
      }

      helper()
    }
  }

  override def sort(comparator: (T, T) => Int): LinkList[T] = {
    def partition(pivot: T, list: LinkList[T]): (LinkList[T], LinkList[T], LinkList[T]) = {
      val leftPred: T => Boolean = e => comparator(e, pivot) < 0
      val rightPred: T => Boolean = e => comparator(e, pivot) > 0
      val samePred: T => Boolean = e => comparator(e, pivot) == 0
      (list.filter(leftPred), list.filter(samePred), list.filter(rightPred))
    }

    @tailrec
    def insertInSorted(toInsert: LinkList[T], sorted: LinkList[T], acc: LinkList[T] = EmptyLink): LinkList[T] = {
      if (sorted.isEmpty) acc ++ toInsert
      else {
        if (comparator(toInsert.head, sorted.head) <= 0) {
          acc ++ (toInsert ++ sorted)
        } else {
          val newSorted = sorted.tail
          val newAcc = acc :+ sorted.head
          insertInSorted(toInsert, newSorted, newAcc)
        }
      }
    }

    @tailrec
    def helper(left: LinkList[T] = this, right: LinkList[T] = EmptyLink, sorted: LinkList[T] = EmptyLink): (LinkList[T], LinkList[T], LinkList[T]) = {
      if (left.isEmpty && right.isEmpty) {
        (left, sorted, right)
      } else {
        val pivot = if (left.isEmpty) {
          right.head
        } else {
          left.head
        }
        val (l, c, r) = partition(pivot, left ++ right)
        val newSorted = insertInSorted(c, sorted)
        helper(l, r, newSorted)
      }
    }

    if (tail.isEmpty) {
      this
    } else {
      val (left, sorted, right) = helper()
      sorted
    }
  }

  override def zip[A](linkList: LinkList[A]): LinkList[(T, A)] = {
    if (linkList.isEmpty) EmptyLink
    else {
      Link((head, linkList.head), tail.zip(linkList.tail))
    }
  }

  override def zipWith[A, B](linkList: LinkList[A], f: ((T, A)) => B): LinkList[B] = {
    if (linkList.isEmpty) EmptyLink
    else {
      Link(f(head, linkList.head), tail.zipWith(linkList.tail, f))
    }
  }

  override def forEach(f: T => Unit): Unit = {
    f(head)
    tail.forEach(f)
  }

  def map[A >: T](mapFunction: T => A): LinkList[A] = {
    @tailrec
    def helper(remainder: LinkList[T] = this, acc: LinkList[A] = EmptyLink): LinkList[A] = {
      if (remainder.isEmpty) {
        acc
      } else {
        helper(remainder.tail, acc :+ mapFunction(remainder.head))
      }
    }

    helper()
  }

  def flatMap[A >: T](flatMapFunction: T => LinkList[A]): LinkList[A] = {
    @tailrec
    def helper(remainder: LinkList[T] = this, acc: LinkList[A] = EmptyLink): LinkList[A] = {
      if (remainder.isEmpty) {
        acc
      } else {
        helper(remainder.tail, acc ++ flatMapFunction(remainder.head))
      }
    }

    helper()
  }

  def filter(predicate: T => Boolean): LinkList[T] = {
    @tailrec
    def helper(remainder: LinkList[T] = this, acc: LinkList[T] = EmptyLink): LinkList[T] = {
      if (remainder.isEmpty) {
        acc
      } else {
        if (predicate(remainder.head)) {
          helper(remainder.tail, acc :+ remainder.head)
        } else {
          helper(remainder.tail, acc)
        }
      }
    }

    helper()
  }

  def ++[A >: T](other: LinkList[A]): LinkList[A] = {
    @tailrec
    def helper(me: LinkList[A] = this, acc: LinkList[A] = other): LinkList[A] = {
      if (acc.isEmpty) {
        me
      } else {
        helper(me :+ acc.head, acc.tail)
      }
    }

    helper()
  }

  override def strRepresentation: String = {
    @tailrec
    def helper(acc: String, remainder: LinkList[T]): String = {
      if (remainder.isEmpty) {
        acc
      } else {
        helper(s"$acc, ${remainder.head}", remainder.tail)
      }
    }

    helper(head.toString, tail)
  }
}
