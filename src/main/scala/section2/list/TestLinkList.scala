package rpt.udemy
package section2.list

import scala.util.Random

object TestLinkList extends App {

  Random.setSeed(0L)

  val aLinkedList: LinkList[Int] = LinkList(
    (0 to 100).map(i => Random.between(-1000, 1000)): _*
  )
  val appendItem = aLinkedList :+ 5

  println(aLinkedList.map(_ * 2))
  println(aLinkedList.flatMap(i => LinkList(i, i * 10)))
  println(aLinkedList.filter(_ % 2 == 0))
  val sortedList = aLinkedList.sort(_ - _)
  println(sortedList)
  println(
    sortedList.foldLeft((true, Int.MinValue)) {
      (check, e) => ((e >= check._2) && check._1, e)
    }
  )
  println(
    aLinkedList.zip(LinkList("a", "b", "c", "d")).map {
      case (i, str) => i.toString + str
    }
  )
  println(
    aLinkedList.zipWith(LinkList("a", "b", "c", "d"), _ + _)
  )

  // For Comprehension test
  val numbers = LinkList(1, 2, 3)
  val letters = LinkList("a", "b", "c")
  val colors = LinkList("white", "black")

  val comprehensiveTest = for {
    n <- numbers
    s <- letters
  } yield s"$n-$s"

  println(comprehensiveTest)

  case class Square(number:Int, letter:String, color:String)
  println(numbers.flatMap(n => letters.flatMap(s => colors.map(c => Square(n,s,c)))))

}
