package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class GameActor extends Actor {
  def receive = {
    case g@Game(_,_) => println("Game between " + g.teamOne +" and  " + g.teamTwo)//calculateWinner(g.teamOne, g.teamTwo)
  }
}