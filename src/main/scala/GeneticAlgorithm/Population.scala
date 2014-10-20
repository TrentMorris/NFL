package trent.nfl

import scala.util.Random

class Population(pop: List[Chromosome]){
	val population: List[Chromosome] = pop
	def size = population.size

	override def toString = population.mkString("\n")
}

object Population{
	def apply(size: Int)(chromoSize: Int) = {
		new Population(List.fill(size)(Chromosome.apply(chromoSize)))
	}
}