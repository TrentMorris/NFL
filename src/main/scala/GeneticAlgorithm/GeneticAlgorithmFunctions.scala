package trent.nfl
import scala.util.Random


trait GeneticAlgorithmScala {

	def mutateChromosome(ch: Chromosome, index :Int): Chromosome = {
		val chList = ch.chromosome
		val newChList = if (index < 18) chList.slice(0, index) ::: List( Random.nextFloat) ::: chList.slice(index + 1, chList.size) ::: Nil
						else 			chList.slice(0, index) ::: List(-Random.nextFloat) ::: chList.slice(index + 1, chList.size) ::: Nil
		new Chromosome(chList)
	}

	def getTopFromPopulation(numberOfBest: Int, values: List[(Int, Int)], pop: Population): List[Chromosome] = {
		val indexes = indexesOfBest(numberOfBest, values)
		for (eachIndex <- indexes) yield pop.population(eachIndex)
	}

	def indexesOfBest(numberOfBest: Int, values: List[(Int, Int)]): List[Int] = {
		if (numberOfBest >= values.size) return List.range(0, values.size)
		def go( sizeSoFar: Int, values: List[(Int, Int)], accum: List[Int]): List[Int] = {
			if (sizeSoFar == numberOfBest) return accum

			val maxCh = values.maxBy(_._2)
			val index = values.indexOf(maxCh)
			val newList = values diff List(maxCh)
			go(sizeSoFar +1, newList, maxCh._1 :: accum)
		}
		go(0, values, List())
	}

	def getBestChromosomeAndScoreFromPopulation(values: List[(Int, Int)], pop: Population): (Chromosome,Int) = {
		val max = values.maxBy(_._2)
		(getChromosomeFromPopulation(pop, values.max._1), max._2)
	}

	// def findMaxValue(values: List[(Int, Int)]): Int = values.maxBy(_._2)._2

	def newPopulationFromOld(pop: Population, bestPop: List[Chromosome], randomToBreed: Int, mutationRate: Double): Population = {
		import scala.util.Random
		val popSize = pop.population.size

		val randomBredWithRandomNumber = (0.3 * popSize).toInt 

		val bestBreedWithRandomPopulation = for (bestCh <- bestPop) // 10%
			yield {
				val index = Random.nextInt(pop.size)
				breedTwoChromosomes(bestCh, pop.population(index))
			}

		val randomBredWithRandom = for (x <- List.range(0,randomBredWithRandomNumber)) // 30%
			yield {
				val firstIndex = Random.nextInt(pop.size)
				val secondIndex = Random.nextInt(pop.size)
				breedTwoChromosomes(pop.population(firstIndex), pop.population(secondIndex))
			}
		val bestMutated: List[Chromosome] = (for (ch <- bestPop) yield {
			if (Random.nextFloat < mutationRate) mutateChromosome(ch, Random.nextInt(ch.size))
			else Chromosome.basicChromosome(35)
			}).filter(x => x.chromosome != Chromosome.basicChromosome(35).chromosome)

		val allButRandom: List[Chromosome] = bestPop ::: randomBredWithRandom ::: bestBreedWithRandomPopulation ::: bestMutated
		val remainingNeeded = popSize - allButRandom.size 
		val finalPop = for (x <- List.range(0, remainingNeeded)) yield Chromosome.apply(35)
		new Population(allButRandom ::: finalPop)
		
	}

	def breedTwoChromosomes(ch1: Chromosome, ch2: Chromosome): Chromosome = {
		val chList = for (index <- List.range(0, ch1.size)) yield {
			if (Random.nextBoolean) ch1.chromosome(index)
			else ch2.chromosome(index)
		}
		new Chromosome(chList)
	}

	def firstGenerationPopulation(popSize: Int, chromoSize: Int) = Population(popSize)(chromoSize)

	def addChromosomeToPopulation(ch: Chromosome, pop: Population): Population = new Population(ch :: pop.population)

	def getChromosomeFromPopulation(pop: Population, index: Int): Chromosome = pop.population(index)

	def resultsToAmountRight(result: List[(Int, Int)], gamesToSort: Int) = {
		val sortedResult = result.sortBy(_._1)
		val slidingResult = sortedResult.sliding(gamesToSort,gamesToSort).toList
		val amountRight = slidingResult.map(x => x.foldLeft(0)((b,a) => b + a._2))
		(List.range(0,100), amountRight).zipped.toList
	}
}
