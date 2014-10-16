package trent.nfl

import scala.util.Random

class Population(pop: List[Chromosome]){
	val population = pop

	// override def toString = population.mkString
}

object Population{
	def apply(size: Int)(chromoSize: Int) = {
		new Population(List.fill(size)(Chromosome.apply(chromoSize)))
	}
}