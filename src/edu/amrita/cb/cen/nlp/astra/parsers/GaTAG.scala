package edu.amrita.cb.cen.nlp.astra.parsers

import scala.util.Random
import scala.annotation.tailrec
import edu.amrita.cb.cen.nlp.astra.util.XTAG

object GaTAG {
 
  val size = 10000
  val rand  = new Random((scala.math.random * size).toInt)
  var randCount = size/100;
  
  val trees = XTAG.treeList	
  
  val primaryPOS = "V"
	
  val gammaI = XTAG.rootGenes.filter(r => r._4 == primaryPOS)
	val gammaC = XTAG.subsGenes ++ XTAG.adjuGenes
	
	val gammaCMap = {
		val l = gammaC.map(g => g._4).distinct
		l.map(a => (a, (0 until gammaC.length).filter(j => gammaC(j)._4 == a))).toMap
  }
  
  val sentence = "N V N P D N".split(' ').toIndexedSeq
	val primaryGenePos = (0 until sentence.length).filter(pos => sentence(pos) == primaryPOS)
	
	var population = createPopulation.zipWithIndex
	
	val ordIndices = (0 until sentence.length).filter(_ != primaryGenePos.head)
	val allIndices = (0 until sentence.length).toIndexedSeq

	val cartIndices = ordIndices.flatMap (i => allIndices.map ( (i,_) ) ).filter(x => x._1 != x._2) 
  	
	def random(n: Int) = {    
    randCount -= randCount    
    if(randCount < 0) {
      randCount = size/100;
      rand.setSeed((scala.math.random * size).toInt)
    }
    rand.nextInt(n)
  }

	// Creates a random population of 'size' number of individuals
	def createPopulation = {
	val p = List[IndexedSeq[Int]]()

		@tailrec
		def reccur(count: Int, p: List[IndexedSeq[Int]]): List[IndexedSeq[Int]] = {
			val k = Array.fill[Int](sentence.length)(-1) 

			for (i <- 0 until sentence.length) {
				k(i) = if (i == primaryGenePos.head) random(gammaI.length)
				else {
					val filterGeneList = gammaCMap(sentence(i))
					filterGeneList(random(filterGeneList.length))
				}
			}
			if (count != 0) reccur(count - 1, k.toIndexedSeq :: p) else p
		}
		reccur(size, p)
	}
   
	
	def howcohere1( k: IndexedSeq[Int] ) = {
	  	  
	  cartIndices.map { i =>
	    val (c,p) = i
	    val pGeneTree = if(p == primaryGenePos.head) gammaI( k(p) )._1 else gammaC( k(p) )._1
	    if ( gammaC( k(c) )._2 == pGeneTree) 1 else 0
	  }.reduce(_+_)	  	  
	}
	
	
	def howcohere2( k: IndexedSeq[Int] ) = {
	  var i = 0
	  var coh = 0
	  
	  while(i < k.length) {
	    if(i == primaryGenePos.head) i += 1
	    else {
	      val x = allIndices.map { p =>	   
	        if((i >= p && (i <= k.length - 2) )|| i == p ) 0 else {
	          val pGeneTree = if(p == primaryGenePos.head) gammaI( k(p) )._1 else gammaC( k(p) )._1
	          if ( gammaC( k(i) )._2 == pGeneTree) 1 else 0
	        }
	      }.reduce(_+_)
	      if(x == 0) i = k.length else coh += x
	      i += 1
	    }	    
	  }	  
	  	coh  
	}	
	
	def howorder( k: IndexedSeq[Int] ) = {
	  
	}	
	
	def selector (population: List[ (IndexedSeq[Int],Int) ], 
	    rate: Float, pAvgCoh: Int): List[ (IndexedSeq[Int],Int) ] = {
	  
	  val cohList = population.map ( k => (howcohere2(k._1), k._2) )
	          .filter( h => h._1.toFloat/sentence.length.toFloat >= rate )	  
	   
	  val avgCoh = if(cohList.isEmpty) pAvgCoh
	               else (cohList.map(_._1).reduce(_+_).toFloat/cohList.length.toFloat).toInt
	  	               
	  println(cohList.length +" "+ rate+ " "+ avgCoh)        
	  // Return new population
	  val newpopulation  = population.map{ kv => 
	    val (k,i) = kv
	    if(cohList.isEmpty) {
	      val r = random(population.length)
	      val newK = psiD_lr(population( r )._1 , k, pAvgCoh)
    	    (psiM_lr(newK, pAvgCoh), i)
	    }
	    else  {
    	    val r = random(cohList.length)
  	      
    	    val newK = psiD_lr(population( cohList(r)._2 )._1 , k, cohList(r)._1 )
    	    random(3) match {
    	      case 1 => (psiM_lr(newK, cohList(r)._1), i)
    	      case _ => (newK, i)
    	    }
	    }
	  }   	       
    	if (avgCoh >= sentence.length - 1) newpopulation 
    	else if(cohList.isEmpty) selector(newpopulation, rate-0.02f, pAvgCoh-1) 
    	     else selector(newpopulation, rate + 0.03f, avgCoh)   
	}	
	
	
  def psiD_lr (k: IndexedSeq[Int], kr: IndexedSeq[Int], coh : Int ) = {
	  val slack = if(coh >= primaryGenePos.head) coh+1 else coh
    
	  if (slack >= k.length) k  
    else {
      val r = random(k.length - slack) + slack    
	    allIndices.map ( i =>  if(i >= r) kr(i) else k(i) )
    }
  }
  
  
  def psiM_lr(k: IndexedSeq[Int],coh: Int) = {
    val slack = if(coh >= primaryGenePos.head) coh+1 else coh
    
    if (slack >= k.length) k 
    else {
      val r = random(k.length - slack) + slack 
      val gList = gammaCMap(sentence(r))
          
      allIndices.map{ i => 
        if(i == r) { 
          if(r == primaryGenePos.head) random(gammaI.length) 
          else gList(random(gList.length))
        }
        else k(i)      
      }    
    }
  }
  
  @inline 
  def isRootGene(g: (Int, Int, Int, String)) = (g._2 < 0)

	@inline
	def printGene(g: (Int, Int, Int, String)) = { 
	  if (isRootGene(g)) println("I-Gene: tree=" + trees(g._1) + " anchor=" + g._4)
		else println("C-Gene: tree=" + 
		  trees(g._1) +
		  " parent=" + 
		  trees(g._2) + 
		  " node(parent)=" + 
		  trees(g._2).nodes(g._3).name + 
		  " anchor=" + g._4)
	}

  @inline
	def printChromosome(k: IndexedSeq[Int]) = {
    println("********************************************************************") 
		(0 until k.length).foreach(g => printGene(if (g == primaryGenePos.head) gammaI(k(g)) else gammaC(k(g))))
	}

	def main(args: Array[String]) = {

//	  val l = gammaCMap.map{kv => val (k,v) = kv; (k,v.size) }.toList
//  l.foreach(println)
	 
    population = selector(population, 0.0f, 0)
	
		val cohList = population.map ( k => (howcohere1(k._1), k._2) )
	         .filter( h => h._1.toFloat/sentence.length.toFloat > .85 )
	  
	  println(cohList.length)
	  population.reverse.take(20).foreach(p => printChromosome(p._1))		
	}
}