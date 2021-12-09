package rpt.udemy
package section2.list

import scala.util.Random

object TestLinkList extends App {

  Random.setSeed(0L)

  val aLinkedList: LinkList[Int] = LinkList(
    (1 to 10000).map(i=>Random.between(-1000,1000)):_*
  )
  val appendItem = aLinkedList :+ 5

  println(aLinkedList.map(_ * 2))
  println(aLinkedList.flatMap(i => LinkList(i, i * 10)))
  println(aLinkedList.filter(_ % 2 == 0))
  println(aLinkedList.sort(_ - _))
  println(
    aLinkedList.zip(LinkList("a", "b", "c", "d")).map {
      case (i, str) => i.toString + str
    }
  )
  println(
    aLinkedList.zipWith(LinkList("a", "b", "c", "d"), _ + _)
  )


  var sum = 0
  appendItem.forEach(e=>sum+e)
}
