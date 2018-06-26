package edu.amrita.cb.cen.nlp.astra.util

import scala.annotation.tailrec
import scala.xml._
import java.text.ParseException
import java.io.BufferedReader
import java.io.FileReader

/**
 * This class is used for piling up string pattern to generate the String representation of some objects.
 * It houses an internal StringBuilder from java.lang package, containing it and wraps the append method
 * in a suitable operator. Mostly used by the Tree node class inside its 'toString' method.
 */

class StringAddable {
	val string = new StringBuilder()

	@inline def <<(a: String) = { string append a; this }
	@inline def <<(a: Char) = { string append a; this }
	@inline def <<(a: Int) = { string append a; this }

	override def toString = string.toString
}

/**
 * ******************************************************************************************************
 * The Annotation class for Trees which holds tree meta data. The class does not have any methods but has
 * certain public parameters and is immutable by design. The decision for decoupling the tree annotations
 * were necessary to initial temporary trees with no annotation baggage.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

case class TreeAnnotation(

		val name: String,
		val pos: String,
		val word: String,
		val treeType: Boolean,
		val family: String,
		val language: String,
		val comment: String,
		val unification: String) {

	val INITIAL = true
			val AUXILIARY = false
}

/**
 * *******************************************************************************************************
 * The main Tree class. The class represents a linguistic tree and is hence the main face of this package
 * among all others. It is a concrete class and once created it is immutable. The class also has a compa-
 * -nion object which houses the major static features and some constants, required by the invariant here
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

case class Tree(val root: TreeNode, val depth: Int, val annotation: TreeAnnotation)
extends Cloneable {

	require(root.isRoot, "The root has to be a root type node!")

	lazy val nodes = Tree.makeListOfNodes(this).toIndexedSeq
	def isInital = annotation.treeType
	override def toString = if (isInital) '\u2510' + annotation.name else '\u2514' + annotation.name

	@inline def isSentence = root.name.charAt(0) == 'S'
	@inline def isInitial = annotation.treeType

	@inline def pos = annotation.pos
	@inline def name = annotation.name
	@inline def word = annotation.word
	@inline def comment = annotation.comment
}

/**
 * *******************************************************************************************************
 * The Companion object of Tree Class. The main feature here is the 'makeTree***' methods that parses tree
 * string representations and produces a Tree object. Trees can be specified in these kinds of exchange
 * formats by users. This also hold necessary invariant constants to maintain programmability.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

object Tree {

  val INITIAL = true
  val AUXILIARY = false
  val NODE_LENGTH = 15
  val NODENAME = "n:"
  val CONSTRAINT = "c:"
  val PROPERTY = "p:"
  val SUBSCRIPT = "s:"
  val LPAREN = '('
  val RPAREN = ')'

	def main(args: Array[String]) = {

	}

	/**
	 * --------------------------------------------------------------------------------------------------
	 * This function will read the tree files from XTAG Grammar and convert them to contemporary trees.
	 * It also reads annotations and assigns proper tags to trees including the sample sentence or the
	 * comment etc...
	 * --------------------------------------------------------------------------------------------------
	 */
	def readXTAGTreeFile(treefile: String) = {
		val in = new BufferedReader(new FileReader(treefile))
		var temp = in.readLine
		var (n, comment, tree, unieq): (String, String, Tree, String) = ("", "", null, "")

		// This is the recursion that reads line after line and process it as required ....
		@tailrec
		def reccur(treelist: List[Tree]): List[Tree] = {
			tree = null
			// Check the first character of the line to decide its processing ....
			if (!temp.trim.isEmpty) temp.charAt(0) match {
				// Open parenthesis is for tree name... 
				case '(' =>
				  if (temp.indexOf('"', 2) > 0)
					  n = temp.substring(2, temp.indexOf('"', 2))
					// quotation are used for comments and other numerous purposes ...
				case '"' =>
				  if (temp.startsWith("\" :COMMENT")) {
					  comment = temp.substring(13)
						if (comment.indexOf("\" :SHAPE") > 0)
							comment = comment.substring(0, comment.indexOf("\" :SHAPE"))
				  }
				// Finally the tree string is in here, sometimes in multiple lines... 	
				case ' ' =>
				  if (temp.indexOf("((((") == 1) {
					  tree = makeTreeFromLISP(temp,
						new TreeAnnotation(n.substring(1), null, null,
							(n.charAt(0) == ''),
							treefile.substring(treefile.lastIndexOf('/') + 1, treefile.lastIndexOf('.')),
							"English", comment, unieq))
							unieq = ""
				  } // Other wise its an unfinished comment ...
				  else if (temp.indexOf("\" :SHAPE") > 0)
					  comment += temp.substring(0, temp.indexOf("\" :SHAPE"))
					  else comment += temp
				case _ =>
				if (temp.indexOf('=') < 0)
					if (temp.indexOf("\" :SHAPE") > 0)
						comment += temp.substring(0, temp.indexOf("\" :SHAPE"))
						else comment += temp
						else unieq += temp + "\n"
	    }
			temp = in.readLine
			  // Check if a file is complete or just a tree is complete, add it to the list ...		
				if (temp != null)
					if (tree != null) reccur(tree :: treelist)
					else reccur(treelist)
				else 
				  if (tree != null) tree :: treelist
				  else treelist
				  
	  }
    reccur(Nil).reverse
	}

	/**
	 * ---------------------------------------------------------------------------------------------------
	 * A tree retrieval function to parse the XTAG Lisp tree notations and create our contemporary tree.
	 * This will be an internally recursive function just like the one that follows. but it can parse trees
	 * from XTAG Grammar '.tree' type files.
	 * ---------------------------------------------------------------------------------------------------
	 */

	def makeTreeFromLISP(tree: String, annot: TreeAnnotation) = {

		// Remove unwanted spaces and quotation marks ...
	  val cTree = tree.replace('"', ' ').replaceAll("\\s+", "")

		// The depth of the tree is an argument to Tree constructor...
		var depth = 0

		// Checking the first 3 parenthesis on a node ...
		def checkLParens(index: Int) = {
			if (cTree.charAt(index) == Tree.LPAREN)
				if (cTree.charAt(index + 1) == Tree.LPAREN)
					if (cTree.charAt(index + 2) == Tree.LPAREN) true
					else false
				else false
			else false
		}

		// The sub function to pull out all node annotations ...
		def getNodeAnnotations(index: Int) = {

			// The Lisp dump is redundant and over complicated for contemporary use ...
			// Many annotations are useless or is not relevant ...
			// So we will mine out only what is required and discard the rest ...
			if (!checkLParens(index))
				throw new IllegalArgumentException("Error in format; bracket missing near : " + cTree.substring(index))

			// indexing name and subscript ...
			val dot_i = cTree.indexOf('.', index + 3)
			val rparen1_i = cTree.indexOf(')', index + 3)
			val rparen3_i = cTree.indexOf(')', rparen1_i + 2)

			// assigning .... name and subscript ...
			val n = cTree.substring(index + 3, dot_i)
			val s = cTree.substring(dot_i + 1, rparen1_i)

			// indexing properties and constraints ....
			val cnull_i = cTree.indexOf("NA", rparen1_i)
			val footn_i = cTree.indexOf("footpT", rparen1_i)
			val anchr_i = cTree.indexOf("headpT", rparen1_i)
			val csubs_i = cTree.indexOf("substpT", rparen1_i)

			// assigning .... constraints ....
			val c = if (cnull_i > 0 && cnull_i < rparen3_i) TreeNode.NULL_ADJUNCTION
			  else if (csubs_i > 0 && csubs_i < rparen3_i) TreeNode.SUBSTITUTION
			  else TreeNode.NIL
			// assigning .... properties ....
			val p = if (anchr_i > 0 && anchr_i < rparen3_i) TreeNode.ANCHOR
			  else if (footn_i > 0 && footn_i < rparen3_i) TreeNode.FOOTNODE
			  else TreeNode.NIL

			// Return the Index of tree string and node annotations ...
			(rparen3_i, n, c, p, s)
		}

		// Get annotations for root node ....
		val (rp3_i, n, c, p, s) = getNodeAnnotations(1)

			// The tail recursive logic to process rest of the tree...
			@tailrec
			def reccur(index: Int, pnode: TreeNode): TreeNode = {
			// We will not use LPAREN, RPAREN constants here as XTAG will not change these notations now...		
			cTree.charAt(index) match {
				// recurse over sibling of current node ...
			  case ')' => if (!pnode.isRoot) reccur(index + 1, pnode.parent)
					          else pnode
				// Process and recurse over child node ....
			  case '(' => {
				  val (rp3_i, n, c, p, s) = getNodeAnnotations(index + 1)
					val newNode = if (n.charAt(0) == '') new EmptyNode(pnode)
						            else new NormNode(n, c, p, s, pnode)
				  if (depth < newNode.depth) depth = newNode.depth
				  reccur(rp3_i + 1, newNode)
			  }
			  case _ => throw new IllegalArgumentException
			}
		}
		// Initiate recursive tree construction with root node ...
		new Tree(reccur(rp3_i + 1, new RootNode(n, c, p, s)), depth, annot)
	}

	/**
	 * ---------------------------------------------------------------------------------------------------
	 * This a tail recursive function that translates the tree notation to a Tree object that the system
	 * manipulates. The function works on the precision of the notations; its spacing and positions of var-
	 * -ious attribute names.
	 * ---------------------------------------------------------------------------------------------------
	 */
	def makeTree(tree: String, annot: TreeAnnotation) = {

		// Get root properties ....
		val n = tree substring (tree.indexOf(NODENAME) + 2, tree.indexOf(" "))
		val c = tree charAt (tree.indexOf(CONSTRAINT) + 2)
		val p = tree charAt (tree.indexOf(PROPERTY) + 2)
		val s = tree.substring(tree.indexOf(SUBSCRIPT) + 2, tree.indexOf(" ", tree.indexOf(SUBSCRIPT) + 2))

		// This keeps track of the tree depth ...
		var depth = 0

		// the real tail recursive function that deals with all other nodes except the root ....
		@tailrec
		def reccur(index: Int, node: TreeNode): TreeNode = {

		  // Decide the current node ....
			if (index >= tree.length || index < 0) node
			// Or recurse on to the next node....
			else if (tree.charAt(index) == RPAREN) reccur(index + 1, node.parent)
			// Separate all attributes of the current node....
			else {
			  val n = tree.substring(tree.indexOf(NODENAME, index) + 2, tree.indexOf(" ", index))
				val c = tree.charAt(tree.indexOf(CONSTRAINT, index) + 2)
				val p = tree.charAt(tree.indexOf(PROPERTY, index) + 2)
				val s = tree.substring(tree.indexOf(SUBSCRIPT, index) + 2, tree.indexOf(" ", tree.indexOf(SUBSCRIPT, index) + 2))

				// Get next token character to process ...
				val next = index + NODE_LENGTH + n.length

				// Create the node according to requirement... 
				val newNode: TreeNode = if (n == "eps") new EmptyNode(node) else new NormNode(n, c, p, s, node)

				// Re-adjust depth after the addition ...
				if (depth < newNode.depth) depth = newNode.depth

				// Tail recurse either back to the parent to create sibling... of stay on to create sub node....
				tree.charAt(next) match {
				  case RPAREN => reccur(next + 1, newNode.parent)
					case LPAREN => reccur(next, newNode)
					case _ => throw new ParseException("Tree format error; Unknow character near:" + tree.charAt(next), next)
				}
		  }
		}
		// Trigger the recursive process by initiating with root node
		new Tree(reccur(NODE_LENGTH + n.length(), new RootNode(n, c, p, s)), depth, annot)
	}

	/**
	 * A Non tail-recursive function for enlisting tree nodes.
	 */
	def makeListOfNodes(tree: Tree) = {

		def reccur(nList: List[TreeNode]): List[TreeNode] =
				nList match {
				case Nil => Nil
				case x :: ys => x :: reccur(x.children ++ ys)
		}
		tree.root :: reccur(tree.root.children)
	}

	/**
	 * ---------------------------------------------------------------------------------------------------
	 * A function returning all trees inside a tree xml file. Trees are, however saved as LISP strings with
	 * in with a tree TAG.
	 * ---------------------------------------------------------------------------------------------------
	 *
	 */
	@deprecated
	def makeTrees(treexmlFile: String): List[Tree] = {
		val treeXML = XML.loadFile(treexmlFile);
		val treeXMLList = (treeXML \ "tree").theSeq.toList

				def processTree(treexml: Node) = {
			val annot = new TreeAnnotation(
					(treexml \ "@name").toString(),
					null,
					null,
					(treexml \ "@type").toString().charAt(0) == 'I',
					(treexml \ "@family").toString(),
					(treexml \ "@language").toString(),
					null,
					null)
			Tree.makeTree(treexml.text, annot)
		}

		@tailrec def reccur(treexmlList: List[Node], treeList: List[Tree]): List[Tree] = {
			if (!treexmlList.isEmpty) reccur(treexmlList.tail, processTree(treexmlList.head) :: treeList)
			else treeList
		}

		reccur(treeXMLList, Nil)
	}
}

/**
 * *****************************************************************************************************
 * The abstract Tree Node class. Tree and composed of tree nodes and here the base type tree node is def-
 * -ined. It has several abstract methods and some concrete methods. The class is the second most import-
 * -ant class in this package. Notice the class parameters and mostly public immutable types that should
 * be set while creating instances.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

abstract class TreeNode(n: String, val constraint: Char, val property: Char, val subscript: String) {

	protected var list: List[TreeNode] = Nil

			def parent: TreeNode
			def isRoot: Boolean
			def isEmpty: Boolean
			def isDerivation: Boolean

			def name = n
			def children: List[TreeNode] = list
			def isLeaf = children.isEmpty
			def index: List[Int]

					lazy val depth = index.size - 1

					def isHead = (property == TreeNode.ANCHOR)

					def isNilProperty = (property == TreeNode.NIL)
					def isNilConstraint = (constraint == TreeNode.NIL)
					def isNilSubscript = (subscript == TreeNode.NIL)

					// The child linking operator; creates a parent->child relation ...
					def ->(cn: TreeNode): TreeNode = {
							cn match {
								// A root node should not become a child ...
								// So we make a normal clone of the root ... 
							case RootNode(_, _, _, _) =>
							list = list ++ List(new NormNode(cn, this))
							// Otherwise add the node to the 'list' of children ...
							case NormNode(_, _, _, _, x) => {
								if (x == this) list = list ++ List(cn)
										// The node should have a parent set before (doubly linked) the linkage ... 
										else throw new IllegalArgumentException("Cant add as child; Node:" +
												this.name + " is not the parent of Node:" + cn.name)
							}
							// Process this like the previous ....
							case EmptyNode(x) => {
								if (x == this) list = list ++ List(cn)
										else throw new IllegalArgumentException("Cant add as child; EmptyNode:" +
												this.name + " is not the parent of Node:" + cn.name)
							}
							// If it is a Derivation Node
							case DerivationNode(_, _, x, _) => {
								if (x == this) list = list ++ List(cn)
										else throw new IllegalArgumentException("Cant add as child; DerivationNode:" +
												this.name + " is not the parent of DerivationNode:" + cn.name)
							}

							}
							this
					}

					override def toString: String = {

									val s = new StringAddable()

									s << Tree.LPAREN << Tree.NODENAME << this.name << " "
									s << Tree.CONSTRAINT << this.constraint << " "
									s << Tree.PROPERTY << this.property << " "
									s << Tree.SUBSCRIPT << this.subscript
									if (!children.isEmpty) for (i <- children) s << i.toString
									s << Tree.RPAREN

									s.toString()
							}
}

/**
 * *****************************************************************************************************
 * A sub class of the parent Tree Node that represents a Root Node. The main property describing a root
 * node is how its parenting is defined. Here we define a root as parent to itself. So the parent method
 * is static and not a class parameter.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */
case class RootNode(n: String, cn: Char, p: Char, s: String) extends TreeNode(n, cn, p, s) {

	val parent = this
			val isRoot = true
			val isEmpty = false
			val isDerivation = false
			val index = List(0)
}

/**
 * A sub class of the parent Tree Node that represents a Normal Node; has a clone constructor.
 */
case class NormNode(n: String, cn: Char, p: Char, s: String, val parent: TreeNode) extends TreeNode(n, cn, p, s) {

	require(
			parent.children match {
			case EmptyNode(_) :: ys => false
			case _ => true
			},
			"Empty Node (found at head) no more children can be added to node:" + parent.name)

	def this(node: TreeNode, parent: TreeNode) = {
		this(node.name, node.constraint, node.property, node.subscript, parent)
		this.list = node.children
	}

	parent -> this
	val isRoot = false
	val isEmpty = false
	val isDerivation = false
	val index: List[Int] = parent.index ++ List(parent.children.size)
}

/**
 * A sub class of the parent Tree Node that represents a Empty Node.
 */
case class EmptyNode(val parent: TreeNode) extends TreeNode("eps", TreeNode.NIL, TreeNode.NIL, TreeNode.EMPTY) {

	require(parent.children.isEmpty, "Empty Node should be a single child to its parent")

	parent -> this
	val index: List[Int] = parent.index ++ List(parent.children.size)
	override def children: List[TreeNode] = Nil
	val isRoot = false
	val isEmpty = true
	val isDerivation = false
	override def ->(cn: TreeNode) = this
}

case class DerivationNode(val index: List[Int], n: String, parnt: TreeNode, word: String)
extends TreeNode(n, TreeNode.NIL, TreeNode.NIL, word) {

	require(parnt == null || parnt.isInstanceOf[DerivationNode],
			"Empty Node should be a single child to its parent")

	val parent = {
		if (parnt != null) {
			parnt -> this
			parnt
		} else this
	}

	val isDerivation = true
			val isRoot = if (parnt == null) true else false
			val isEmpty = false
}

object TreeNode {
	val NULL_ADJUNCTION = 'N'
	val SUBSTITUTION = 'S'
	val OBLIGATORY = 'O'
	val SELECTIVE = 'A'
	val NIL = 'e'
	val FOOTNODE = 'f'
	val ANCHOR = 'a'
	val LEXICAL = 'l'
	val EMPTY = ""

	def pcMap(ch: Char) = ch match {
			case NULL_ADJUNCTION => "null-adjuction"
			case SUBSTITUTION => "substitution"
			case OBLIGATORY => "obligatory-adjuction"
			case SELECTIVE => "selective-adjuction"
			case NIL => "empty"
			case FOOTNODE => "footnode"
			case ANCHOR => "headnode"
			case LEXICAL => "lexicon"
	}
}