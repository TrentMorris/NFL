package trent.nfl

import org.specs2.mutable.Specification
import akka.actor._
import akka.testkit._
import akka.util.duration._
import akka.util.Duration
import org.specs2.mutable._
import org.specs2.time.NoTimeConversions

abstract class AkkaTestkitSpecs2Support extends TestKit(ActorSystem()) with After with ImplicitSender {
  def after = system.shutdown()
}

class ActorSpec extends Specification with NoTimeConversions {
  sequential 
   "A WeekActor" should {
	    "send back the game list when asked for it" in new AkkaTestkitSpecs2Support {
	   		val weekActor = system.actorOf(Props[WeekActor])
	      	within(1 second) {
	        	weekActor ! GiveResults
	        	expectMsgType[List[(Int,Int)]] must be equalTo List()
	      	}
	    }
	   "when sent a game add to the list and then be in the list when asked for it" in new AkkaTestkitSpecs2Support {
	   		val weekActor = system.actorOf(Props[WeekActor])   	
	   		within(3 seconds) {
	   			weekActor! GAGameResult(1,100)
	   			weekActor ! GiveResults
	   			expectMsgType[List[(Int,Int)]] must be equalTo List((1,100))
	   		}
	   	}
  	}
  	"A GameActor" should {
  		"take in game parameters, make a prediction, and send a winner back" in new AkkaTestkitSpecs2Support {
  			within(3 seconds){
	  			val gameActor = system.actorOf(Props[GameActor])
	  			gameActor ! GAGame(1,"Pittsburgh Steelers", "Cleveland Browns", Chromosome.basicChromosome(35), 6,3,"2013")
	  			expectMsgType[GAGameResult] //must be equalTo List((1,1))
	  		}
  		}
  	}
  	"MasterActor" should {
  		"send back the game list when asked for it" in new AkkaTestkitSpecs2Support {
	   		val MasterActor = system.actorOf(Props(new Master()))
	      	within(1 second) {
	        	MasterActor ! GiveResults
	        	expectMsgType[List[(Int,Int)]] must be equalTo List()
	      	}
	    }
  	}
}