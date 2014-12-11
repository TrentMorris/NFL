package trent.nfl

import scala.util.Random

class Chromosome(chromo: List[Float]){
	val chromosome = chromo

	override def toString = chromosome.mkString(", ")

	def size: Int = chromosome.size
}

object Chromosome{

	// 18 is beginning of defense

	def apply(size: Int) = {
		val floatList = for (index <- List.range(0,size))yield {
			if (index < 18) Random.nextFloat
			else -(Random.nextFloat)
		}
		new Chromosome(floatList)
	}

	def basicChromosome(size:Int) = {
		val floatList = for (index <- List.range(0,size))yield {
			if (index < 18) 1.toFloat
			else -(1.toFloat)
		}
		new Chromosome(floatList)
	}
	def allZeroChromosome(size: Int) = {
		new Chromosome(List.fill(size)(0.toFloat))
	}
}