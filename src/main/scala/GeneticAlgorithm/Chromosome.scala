package trent.nfl

import scala.util.Random

class Chromosome(chromo: List[Float]){
	val chromosome = chromo

	override def toString = chromosome.mkString
}

object Chromosome{

	// 18 is beginning of defense

	def apply(size: Int) = {
		new Chromosome(List.fill(size)(Random.nextFloat))
	}

	def basicChromosome() = {
		new Chromosome(List.fill(35)(1.toFloat))
	}
	def allZeroChromosome() = {
		new Chromosome(List.fill(35)(0.toFloat))
	}
}