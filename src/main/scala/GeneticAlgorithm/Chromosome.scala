package trent.nfl

import scala.util.Random

class Chromosome(chromo: List[Float]){
	val chromosome = chromo
}

object Chromosome{
	def apply() = {
		new Chromosome(List.fill(35)(Random.nextFloat))
	}

	def basicChromosome() = {
		new Chromosome(List.fill(35)(1.toFloat))
	}
	def allZeroChromosome() = {
		new Chromosome(List.fill(35)(0.toFloat))
	}
}