package rpt.udemy
package section4

import scala.annotation.tailrec

case class SocialNetwork(graph: Map[String, Set[String]] = Map()) {

  def add(person: String): SocialNetwork = {
    if (graph.contains(person)) {
      this
    } else {
      SocialNetwork(graph + (person -> Set()))
    }
  }

  def remove(person: String): SocialNetwork = {
    val friends = friendsOf(person)
    SocialNetwork(
      friends.foldLeft(this)((nn: SocialNetwork, p: String) => nn.unfriend(p, person)).graph - person
    )
  }

  def friend(personA: String, personB: String): SocialNetwork = {
    val friendsA = friendsOf(personA) + personB
    val friendsB = friendsOf(personB) + personA
    SocialNetwork(graph.concat(Map(personA -> friendsA, personB -> friendsB)))
  }

  def friendsOf(person: String): Set[String] = graph.getOrElse(person, Set())

  def unfriend(personA: String, personB: String): SocialNetwork = {
    val newGraph = graph + (personA -> (friendsOf(personA) - personB))
    SocialNetwork(newGraph + (personB -> (friendsOf(personB) - personA)))
  }

  def peopleCount: Int = graph.size

  def friendsCount(person: String): Int = friendsOf(person).size

  def mostSociable: Set[String] = {
    graph.view.mapValues(_.size).groupBy(_._2).toSeq.maxByOption(_._1).map(_._2.map(_._1).toSet).getOrElse(Set())
  }

  def unsociablePeopleCount: Int = graph.count(_._2.isEmpty)

  def connectionCheck(personA: String, personB: String): Boolean = {
    @tailrec
    def helper(personA: String, personB: String, status: Boolean = false, visited: Set[(String, String)] = Set()): Boolean = {
      if (status) {
        status
      } else {
        val friendsA = friendsOf(personA)
        val friendsB = friendsOf(personB)
        if ((friendsA + personA).intersect(friendsB + personB).nonEmpty) {
          true
        } else {
          val newCandidate = friendsA.flatMap(
            friendA => friendsB.map(
              friendB => (friendA, friendB)
            ).filterNot(pair => visited.contains(pair) || visited.contains(pair.swap))
          ).headOption
          if (newCandidate.isEmpty){
            false
          } else {
            val (pA, pB) = newCandidate.get
            val visitedPair = (personA,personB)
            helper(pA, pB, status, visited + visitedPair)
          }
        }
      }
    }

    helper(personA, personB)
  }

  override def toString: String = {
    "\nSocial Network\n==============\n" + graph.mkString("\n")
  }

}
