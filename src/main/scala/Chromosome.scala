package trent.nfl

import scala.util.Random

class Chromosome(chromo: List[Float]){
	val chromosome = chromo
}

object Chromosome{
	def apply() = {
		new Chromosome(List.fill(35)(Random.nextFloat))
	}
}