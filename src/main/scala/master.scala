package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

case object Start

class Master extends Actor {
	def receive =  {

		case Start => println("placeholder")
	}
}