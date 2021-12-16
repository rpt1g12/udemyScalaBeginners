package rpt.udemy
package section4

object SocialNetworkApp extends App {

  val socialNetwork = {
    SocialNetwork().add(
      "Rafa"
    ).add(
      "Guille"
    ).add(
      "Hannah"
    ).friend(
      "Rafa", "Hannah"
    ).friend(
      "Rafa", "Guille"
    ).friend(
      "Guille", "Marta"
    ).add("Javi")
  }
  println(socialNetwork)

  println(socialNetwork.unfriend("Guille", "Marta"))

  println(socialNetwork.connectionCheck("Rafa", "Marta"))
  println(socialNetwork.unfriend("Guille", "Marta").connectionCheck("Rafa", "Marta"))

  println(socialNetwork.friendsCount("Rafa"))
  println(socialNetwork.mostSociable)
  println(socialNetwork.unfriend("Guille", "Marta").unsociablePeopleCount)

}
