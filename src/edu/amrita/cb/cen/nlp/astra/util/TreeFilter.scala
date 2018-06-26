package edu.amrita.cb.cen.nlp.astra.util

import java.io.{ BufferedReader, FileReader }

object TreeFilter {
 
  val treeFreqIter = scala.io.Source.fromFile("xtag/english/corpus-data/unigram.txt")
                  .getLines
                  .filter( l => !l.trim.isEmpty 
                                && l.charAt(0) != ';' 
                                && !l.endsWith("(") 
                                && !l.endsWith("))") )
                  .map( l=> l.replace('(', ' ').replace(')', ' ').replace('"', ' ').trim)
                  .map{ l => val str = l.split("\\s+"); (str(0).toInt, str(1).trim) }.toList    
  
 
  val treeNameList = treeFreqIter.map(tf => tf._2)
  
  
  def main(agrs : Array[String]) {
                   
    println(treeNameList.length)
    val treeList = XTAG.treeList.map( t => t.name )
    val crosList = treeNameList.filter(n => !(treeList contains n) )
    crosList.foreach(println)
    
  }
  
 
}