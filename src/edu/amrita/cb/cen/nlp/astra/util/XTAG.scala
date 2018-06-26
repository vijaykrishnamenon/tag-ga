package edu.amrita.cb.cen.nlp.astra.util

import scala.xml.{ Elem, PrettyPrinter }
import java.io.{ File, PrintStream }
import scala.annotation.tailrec

object XTAG {
 
 val treeList = enlistXTAG("xtag/english/grammar")
     //.filter(t => TreeFilter.treeNameList contains t.name)
 val minTreeIndexList = minimiseXTAG
 val (initialMinTreeIndexList, auxiliaryMinTreeIndexList) = partitionMinimalXTAG
 val (sInitialMinTreeIndexList, pInitialMinTreeIndexList) = partitionMinimalInitialXTAG
 val rootGenes = makeInitialGenePool
 val subsGenes = makeSubsGenePool
 val adjuGenes = makeAdjuGenePool

 @inline def enlistXTAG(xtagpath: String) =
  XTAGViewer.fileListGetter(xtagpath)
   .toIndexedSeq.map(f => f.getPath)
   .flatMap(fn => Tree.readXTAGTreeFile(fn)).sortBy(t => t.name)

 @inline def minimiseXTAG =
  (0 until treeList.length).
   filter(treeList(_).nodes.filter(_.isHead).length < 2)

 @inline def partitionMinimalXTAG =
  (0 until treeList.length)
   .filter(minTreeIndexList contains _)
   .partition(treeList(_).isInital)

 @inline def partitionMinimalInitialXTAG =
  (0 until treeList.length)
   .filter(initialMinTreeIndexList contains _)
   .partition(treeList(_).isSentence)

 @inline def cartesian[T, U](a: IndexedSeq[T], b: IndexedSeq[U]): IndexedSeq[(T, U)] =
  a.map(p => b.map(o => (p, o))).flatten

 @inline def makeInitialGenePool =
  sInitialMinTreeIndexList.map(i => (i, -1, -1, treeList(i).nodes.find(_.isHead).head.name))

 @inline def makeSubsGenePool =
  cartesian((0 until minTreeIndexList.length),
   (0 until initialMinTreeIndexList.length))
   .filter { p =>
    val (i, j) = p
    treeList(i).nodes.filter(_.constraint == TreeNode.SUBSTITUTION)
     .map(_.name) contains treeList(j).root.name
   }
   .flatMap { p =>
    val (i, j) = p
    var tn = 0;
    val subNodes = treeList(i).nodes.filter(_.constraint == TreeNode.SUBSTITUTION)
     .filter(_.name == treeList(j).root.name)
    subNodes.map { n =>
     val ni = treeList(i).nodes.indexOf(n)
     (j, i, ni, treeList(j).nodes.find(_.isHead).head.name)
    }
   }

 @inline def makeAdjuGenePool =
  cartesian((0 until minTreeIndexList.length),
   (0 until auxiliaryMinTreeIndexList.length))
   .filter { p =>
    val (i, j) = p
    treeList(i).nodes.filter(_.constraint == TreeNode.NIL)
     .map(_.name) contains treeList(j).root.name
   }.flatMap { p =>
    val (i, j) = p
    val subNodes = treeList(i).nodes.filter(_.constraint == TreeNode.NIL)
     .filter(_.name == treeList(j).root.name)
    subNodes.map { n =>
     val ni = treeList(i).nodes.indexOf(n)
     (j, i, ni, treeList(j).nodes.find(_.isHead).head.name)
    }
   }

 def XTAGml = {

  var uni: Array[String] = null

  def nodeMl(n: TreeNode): Elem = {
   val c = n.children

   val nt = uni.filter(_.startsWith(n.name
    + (if (n.isNilSubscript) "" else "_" + n.subscript) + ".t"))

   val nb = uni.filter(_.startsWith(n.name
    + (if (n.isNilSubscript) "" else "_" + n.subscript) + ".b"))

   if (c.isEmpty && nb.isEmpty && nt.isEmpty) {
    <node name={ n.name } subscript={ n.subscript.toString } property={ TreeNode.pcMap(n.property) } constraint={ TreeNode.pcMap(n.constraint) } index={
     val i = n.index.toString;
     i.substring(i.indexOf('(') + 1, i.indexOf(')')).replaceAll(", ", ".")
    }/>
   } else {
    <node name={ n.name } subscript={ n.subscript.toString } property={ TreeNode.pcMap(n.property) } constraint={ TreeNode.pcMap(n.constraint) } index={
     val i = n.index.toString;
     i.substring(i.indexOf('(') + 1, i.indexOf(')')).replaceAll(", ", ".")
    }>
     {
      if (!nt.isEmpty || !nb.isEmpty)
       <fs>
        {
         if (!nt.isEmpty)
          <top>{
           nt.map { f =>
            <f key={ f.substring(f.indexOf('[') + 1, f.indexOf(']')) } value={ f.substring(f.indexOf('=') + 2) }/>
           }
          }</top>
        }
        {
         if (!nb.isEmpty)
          <bottom>{
           nb.map { f =>
            <f key={ f.substring(f.indexOf('[') + 1, f.indexOf(']')) } value={ f.substring(f.indexOf('=') + 2) }/>
           }
          }</bottom>
        }
       </fs>
     }
     { if (!c.isEmpty) c.map(nodeMl(_)) }
    </node>
   }
  }

  <xtag language="English" type="Wide Coverage">
   <nonterminals>
    {
     val lexsym = List("by", "to", "eps", "of", "for")
     treeList.flatMap { t =>
      t.nodes.distinct
       .filter(n => !(lexsym contains n.name))
       .map(n => n.name)
     }.distinct
      .map(n => <symbol name={ n }/>)
    }
   </nonterminals>
   <trees>
    {
     treeList.map { t =>
      <tree name={ t.toString } family={ t.annotation.family } pos={
       val i = t.nodes.filter(_.isHead).map(n => n.name).toString
       i.substring(i.indexOf('(') + 1, i.indexOf(')')).replaceAll(", ", "|")
      } type={ if (t.isInital) "Initial" else "Auxiliary" } minimal={ if (t.nodes.filter(_.isHead).length < 2) "yes" else "no" } id={ treeList.indexOf(t).toString }>
       <comment>{ t.comment.replace('\t', ' ').replace('\\', ' ') }</comment>
       <notation>{ t.root.toString }</notation>
       {
        if (!t.annotation.unification.isEmpty) {
         <unification>{
          uni = t.annotation.unification
           .replace('<', '[')
           .replace('>', ']')
           .split('\n')
           .filter(_.indexOf('[') > 0)
          uni.map { e =>
           <eq key={ e.substring(0, e.indexOf('=') - 1) } value={ e.substring(e.indexOf('=') + 1, e.length()).trim }/>
          }
         }</unification>
        } else uni = Array[String]()
       }
       <nodes>{ nodeMl(t.root) }</nodes>
      </tree>
     }
    }
   </trees>
  </xtag>
 }

 def main(xtagDir: Array[String]) = {

  println("\n\n                Total trees :" + treeList.length)
  println("              Minimal trees :" + minTreeIndexList.length)
  println("      Initial Minimal Trees :" + initialMinTreeIndexList.length)
  println("    Auxiliary Minimal Trees :" + auxiliaryMinTreeIndexList.length)
  println(" Sentence Initial Min Trees :" + sInitialMinTreeIndexList.length)
  println("   Phrase Initial Min Trees :" + pInitialMinTreeIndexList.length)
  println("               Initial Gene :" + rootGenes.length)
  println("    Substitutiion Gene Pool :" + subsGenes.length)
  println("       Adjunction Gene Pool :" + adjuGenes.length)

  println("        Total Substitutions :" +
   minTreeIndexList.flatMap {
    treeList(_).nodes
     .filter(_.constraint == TreeNode.SUBSTITUTION)
   }.length)

  println("          Total Adjunctions :" +
   minTreeIndexList.flatMap {
    treeList(_).nodes.filter(
     _.constraint == TreeNode.NIL)
   }.length)

  val features = treeList
   .flatMap(t => t.annotation.unification.split("\n"))
   .filter(f => (f.indexOf('<') > 0))
   .map(f => f.substring(f.indexOf('<') + 1, f.indexOf('>'))).distinct

  println("                   features :" + features.length)
  //features.foreach(println)

//  val path = "/Users/vijaykrishnamenon/Documents/Text/"
//  val xtagxml = new PrintStream(new File(path + "xtag-grammar-english-complete.xml"))
//  val printer = new PrettyPrinter(160, 1)
//  xtagxml.
//  println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + printer.format(XTAGml))
//  xtagxml.close
 }
/* 5706
1-1471317351
MENON
VIJAYKRISHNA
22-Mar-83
1-2449920071
AMRITA VISWA VIDYAPEETHAM
Tamil Nadu
ENGINEERING AND TECHNOLOGY */
}