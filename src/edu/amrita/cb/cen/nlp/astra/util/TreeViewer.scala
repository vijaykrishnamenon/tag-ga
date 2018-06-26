
package edu.amrita.cb.cen.nlp.astra.util

import javax.swing._
import javax.swing.border._
import javax.swing.event.{ ListSelectionListener, ListSelectionEvent, InternalFrameEvent}
import javax.imageio.ImageIO
import java.awt.{ Color, Font, Graphics, Graphics2D, BasicStroke, Frame, Dimension }
import java.awt.geom.Line2D
import java.awt.image.BufferedImage
import java.awt.event.{MouseAdapter, MouseEvent}
import java.io.File
import javax.swing.event.InternalFrameAdapter

object TreeViewer {

	// These constants are just values and can be publicly accessed ...
	// There are no mutable objects among them ...
	val CELLWIDTH = 120
	val CELLHEIGHT = 50
	val CELLGAP = CELLHEIGHT * 2 / 3
	val CELLSPACE = CELLWIDTH / 20
	val XMARGIN = CELLSPACE * 2
	val YMARGIN = CELLGAP
	val BCOLOR = Color.WHITE
	val FCOLOR = Color.DARK_GRAY
	val SBCOLOR = Color.DARK_GRAY
	val SFCOLOR = Color.WHITE
	
	val ANCRCOLOR = Color.BLACK
	val SUBSCOLOR = new Color(0,150,0)
	val NULLCOLOR = new Color(200,0,0)
	val FOOTCOLOR = new Color(0,0,200)
	val EMPTCOLOR = Color.LIGHT_GRAY
	val LEXICOLOR = Color.MAGENTA
	
	val NODEGLASS = true
	val FONT = "Unifont"
	val FONTSTYLE = Font.BOLD
	val FONTSIZE = 25
	val FILLCOLOR = new Color(240,240,240)
	val NODE_BORDER = BorderFactory.createLineBorder(FILLCOLOR) 
	val LINESTROKE = new BasicStroke(2 , BasicStroke.CAP_BUTT,BasicStroke.JOIN_BEVEL,0, Array( 6.5f ), 0)
	val XSIZE = 300
	val YSIZE = 300
		
	val TITLE = "Astra TAG viewer v1.0"
	// The flag that decides which type of view to go with 
	private var viewer = true	
	// Setting basic look and feel
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

	// This Desktop Pane is evaluated only when required. If the listed view is called then there is no 
	// need for this this particular component and hence not evaluated. 
	private lazy val v = new JDesktopPane() {
		// Initialize a Window ...
		val m = new JFrame ( TreeViewer.TITLE )
		
		// Set the properties as you want ...
		m.setExtendedState(Frame.MAXIMIZED_BOTH)
		m.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE)
		
		// Initialize position of the first window with in the Pane...
		private var treePos = (0, 0)
		
		// Further window properties...
		this.setBackground(Color.BLACK)
		m.setContentPane(this)
		
		// A simple operator to add internal documents to the MDI...		
		def <= (tree: TreeCanvas) = {
			// Set tiled location of the internal frame ...
			tree.setLocation(treePos._1, treePos._2)
			
			// Increment Position tuple ...
			treePos = (treePos._1 + XSIZE + 1, treePos._2)
			
			// Warp window to next row it no space at end....
			if (treePos._1 + XSIZE > getWidth()) treePos = (0, treePos._2 + YSIZE + 1)
			
			// Physically add the window to the pane for rendering...
			this.add(tree)
		}
		
		// Set the Main window visible ...
		m.setVisible(true)
	}	
		
	// An inline function to insert newer tree to the MDI ... 
	@inline def draw(tree: Tree): Unit = if (viewer) v <= new TreeCanvas(tree)
				else throw new IllegalStateException(
						"List Viewer is showing... Cannot Open tile viewer")  
	
	
	/**
	 * This function activates the list view of  a list of trees passed. Once list view is enabled by 
	 * calling, then MDI view cannot be opened, in the same VM as the 'viewer' will bind to false.
	 */
	def launchListSeer(treeList: List[Tree]) = {
		if (viewer) viewer = false		
		
		// Create main window ...
		val m = new JFrame ( TreeViewer.TITLE )
		
		// Set its properties ...
		m setExtendedState Frame.MAXIMIZED_BOTH
		m setDefaultCloseOperation JFrame.EXIT_ON_CLOSE
		
		// Initialize the JList component with tree objects in the tree list...
		val list = new JList[Tree](treeList.toArray[Tree])
		
		// Set List properties....
		list setSelectionMode ListSelectionModel.SINGLE_SELECTION
		list setFont new Font(TreeViewer.FONT , TreeViewer.FONTSTYLE , 20)
		
		// Create a split pane for list and the tree Component ...
		val splitPane = new JSplitPane
		
		// Scroll panes for both splits ...
		val scrollPaneR = new JScrollPane
		val scrollPaneL = new JScrollPane
		scrollPaneL setViewportView list
		
		// This list is initialized after the window is visible and rendered....
		// It takes a bit of time to create so many tree components, hence its lazy evaluated ....
		lazy val panelList: List[TreePanel] = for(e <- treeList) yield new TreePanel(e)
		
		// add split pane to main window ..
		m setContentPane splitPane
		
		// Initialize splits with scroll Panes ...
		splitPane setLeftComponent scrollPaneL
		splitPane setRightComponent scrollPaneR
		
		// DEfine the List selection Event response ...
		list.addListSelectionListener(new ListSelectionListener() {
			// Reset Right Pane with the selected tree ...
			def valueChanged ( evt: ListSelectionEvent ) {             
				scrollPaneR.setViewportView (
					panelList ( list.getSelectedIndex ) 
				)
      }
		})
		
		// Make main window visible ...
		m setVisible true
		
		// Set selected Index ....
		list setSelectedIndex 0
		
		// Set View port visible ...
		scrollPaneR setViewportView panelList.head 
	}	
}

/**
 * The Canvas is an Internal Frame that houses the tree panel over a scroll pane in a many document 
 * desktop pane (MDI). It contains all the tree related events with in it, like a packed entity.
 */
class TreeCanvas(tree: Tree) extends JInternalFrame(
		// Set the Title of the Internal Frame based on tree name ... 
		// Also handle the null annotation case ...
		tree.annotation match {
			case null => "some tree"	
			case TreeAnnotation("", _, _, _, _, _, _, _) => "No Name"
			case _ => tree.annotation.name 
		} )  {
	// The tree component which will draw the tree... 
	val t = new TreePanel(tree)	

	// Scroll Panes and Properties for the tree Panel...
	val s = new JScrollPane()
	s.setViewportView(t)
	
	// Internal Frame properties set ...
	this.setResizable(true)
	this.setClosable(true)
	this.setMaximizable(true)
	this setSize (TreeViewer.XSIZE, TreeViewer.YSIZE)

	// Add Tree component to the Internal Frame and make it visible ....
	this.add(s)
	
	this.validate
	this.setVisible(true)
}

/**
 * The main Component that draws and renders the nodes of the physical tree object. The tree is 
 * rendered as a facade and approximates a syntactic view of the grammar construct. The panel 
 * can be enlisted in list components as its string representation will be the tree name. 
 */
class TreePanel(val tree: Tree) extends JPanel {

	// The basic panel properties being set ...
	this setOpaque true
	this setAutoscrolls true
	this setFont (new Font(TreeViewer.FONT, Font.ITALIC, 20))
	this setBackground TreeViewer.BCOLOR
	this setForeground TreeViewer.FCOLOR
	this setLayout null
	this setDoubleBuffered true

	// The edge list is a set of lines that connect the nodes ....
	val edgeList = mapNodes
	
	// These are invariant for event delegations .... 
	private var listeners: List[NodeSelectionListener] = Nil
	private var selected: NodeLabel = null
	
	// The mouse click event resets the selection on nodes...
	this.addMouseListener(new MouseAdapter() {
		
		override def mouseClicked(evt: MouseEvent) = {
			select(null)
		}
	})
	
	// Adding custom node selection event listeners ....
	def addNodeSelectionListener(listener: NodeSelectionListener) = 
		listeners  = listener :: listeners 
	
	// the event fire method ...
	private def fireNodeSelection(evt: NodeSelectionEvent) = {
		new Thread("Node Event Thread") {
			override def run = { for (e <- listeners) e.nodeSelected(evt) }
		}.start()
		
	}
	
	// This method sets node selection and rendering ...
	// It also fires the 'node selected' event...
	def select(nodeLabel: NodeLabel) = {
		if(selected != null) {
			selected setBorder null
			selected setOpaque !TreeViewer.NODEGLASS 
		}
		
		if(nodeLabel != null) { 
			nodeLabel setBorder TreeViewer.NODE_BORDER 
			nodeLabel setOpaque TreeViewer.NODEGLASS
		}		
		
		selected = nodeLabel		
		fireNodeSelection( new NodeSelectionEvent (selected) )
	}
	
	private def mapNodes = {

		val nodes = tree.nodes 

		var xx = TreeViewer.XMARGIN
		var yy = TreeViewer.YMARGIN
		var t: NodeLabel = null
		var i = 0
		var map: Map[TreeNode, (NodeLabel, Int, Int, Int)] = Map()
		var breadth = 0 
		var maxx = 0

		def spanChild(node: TreeNode, x: Int) = {
			if (node.isRoot) 0
			else {
				val parentPosX = map(node.parent)._3 +
					(node.index.last - 1) *
					(TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE)
				if (x < parentPosX)
					parentPosX
				else x
			}
		}

		def spanParent(node: TreeNode, x: Int): Int = {
			if (node.children.isEmpty) x
			else {
				val firstChildPosX = map(node.children.head)._3
				val lastChildPosX = map(node.children.last)._3
				(firstChildPosX + lastChildPosX) / 2
			}
		}

		while (i <= tree.depth) {
			for (node <- nodes if node.depth == i) {
				t = new NodeLabel(node)
				xx = spanChild(node, xx)
				map = map + (node -> (t, breadth, xx, yy))
				xx += TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE
				breadth += 1
			}
			xx = TreeViewer.XMARGIN; breadth = 0
			yy += TreeViewer.CELLHEIGHT + TreeViewer.CELLGAP
			i += 1
		}

		i = tree.depth
		var prevX = TreeViewer.XMARGIN - (TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE)
		var tup: (NodeLabel, Int, Int, Int) = null

		while (i >= 0) {
			for (node <- nodes if node.depth == i) {
				tup = map(node)
				xx = spanParent(node, tup._3)
				if (prevX + TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE > xx)
					xx = prevX + TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE
				prevX = xx
				
				tup._1.setBounds(xx, tup._4, TreeViewer.CELLWIDTH, TreeViewer.CELLHEIGHT)
				
				maxx = if (xx > maxx) xx else maxx
				map = map + (node -> (tup._1, tup._2, xx, tup._4))
				this.add(tup._1)
			}
			prevX = TreeViewer.XMARGIN - (TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE)
			i -= 1
		}

		var bottom: (Int, Int) = (0, 0)
		var top: (Int, Int) = (0, 0)
		var edge: List[(Int, Int, Int, Int)] = Nil

		for (node <- nodes.tail) {
			tup = map(node)
			bottom = topEdge(tup._3, tup._4)

			tup = map(node.parent)
			top = bottomEdge(tup._3, tup._4)

			edge = (top._1, top._2, bottom._1, bottom._2) :: edge
		}
		tup = map(tree.root)
		
		val ydim = (tree.depth + 1) * (TreeViewer.CELLHEIGHT +TreeViewer.CELLGAP) + TreeViewer.YMARGIN 
		val xdim = maxx + TreeViewer.CELLWIDTH + TreeViewer.CELLSPACE    
		
		this.setPreferredSize( new Dimension(xdim, ydim) )
		
		edge
	}

	@inline private def topEdge(x: Int, y: Int): (Int, Int) = {
		(x + TreeViewer.CELLWIDTH / 2, y)
	}

	@inline private def bottomEdge(x: Int, y: Int): (Int, Int) = {
		(x + TreeViewer.CELLWIDTH / 2, y + TreeViewer.CELLHEIGHT)
	}

	override def paintComponent(g: Graphics) = {
		super.paintComponent(g)
		val g2D = g.asInstanceOf[Graphics2D]
		g2D.setStroke(TreeViewer.LINESTROKE)

		for (edge <- edgeList) g2D.draw(new Line2D.Float(edge._1, edge._2, edge._3, edge._4))
	}	
	
	def printTree = {
		val bi = new BufferedImage( 700,700, BufferedImage.TYPE_INT_ARGB); 
    val g: Graphics = bi.createGraphics;
    this.paintComponent(g);  //this == JComponent
    g.dispose();
    try{ ImageIO.write(bi,"png",new File("test.png"));}
    catch { 
    	case e: Exception => println("error")   
    }	
	}
	
	override def toString = "  "+tree.toString
}

class NodeLabel(val node: TreeNode) extends JLabel {

	this setOpaque !TreeViewer.NODEGLASS
	this setFont new Font(TreeViewer.FONT, TreeViewer.FONTSTYLE, TreeViewer.FONTSIZE)
	this setFocusable true

	this setForeground colourMap
	this setBackground TreeViewer.FILLCOLOR

	this setHorizontalAlignment SwingConstants.CENTER
	this setVerticalAlignment SwingConstants.CENTER
	this setBorder null	
	
	this.addMouseListener(new MouseAdapter() {
		
		override def mouseClicked(evt: MouseEvent) = {
			val nodeLabel = evt.getSource.asInstanceOf[NodeLabel]
			nodeLabel.getParent.asInstanceOf[TreePanel].select(nodeLabel)
		}
	})
	
	def colourMap = {
		if (!node.isNilProperty)
			node.property match {
				case TreeNode.ANCHOR => TreeViewer.ANCRCOLOR
				case TreeNode.FOOTNODE => TreeViewer.FOOTCOLOR
				case TreeNode.LEXICAL => TreeViewer.LEXICOLOR
		    }		
		else if (!node.isNilConstraint) 
			node.constraint match {
				case TreeNode.NULL_ADJUNCTION => TreeViewer.NULLCOLOR
				case TreeNode.SUBSTITUTION => TreeViewer.SUBSCOLOR
			} 
		else if (node.isEmpty) TreeViewer.EMPTCOLOR
		else if (node.isLeaf) TreeViewer.LEXICOLOR
		else TreeViewer.FCOLOR
	}
		
	
	val c = node.constraint match {
		case TreeNode.NULL_ADJUNCTION => "\u2349"
		case TreeNode.SUBSTITUTION => "\u21E9"
		case TreeNode.SELECTIVE => "s"
		case TreeNode.OBLIGATORY => "o"
		case _ => ""
	}

	val p = node.property match {
		case TreeNode.ANCHOR => "\u2693"
		case TreeNode.FOOTNODE => "*"
		case TreeNode.LEXICAL => "\u2112"
		case _ => ""
	}

	val s = node.subscript

	this.setText("<html>" +
		(if (node.name == "eps") "\u0190"
		else ("<sup font><b>" + p + "</sup>" +
			node.name + "<sub>" + s + "</sub>"
			+ "<sup><b>" + c + "</sup>") +
			"</html>"))
}

trait NodeSelectionListener {	
	def nodeSelected (evt: NodeSelectionEvent): Unit
}

class NodeSelectionEvent (nodeLabel: NodeLabel) {
	
	@inline def fullNodeName = nodeLabel.node.name + ( 
					if(nodeLabel.node.isNilSubscript) ""
					else "_"+nodeLabel.node.subscript   )
	def node = nodeLabel.node 
	def isNull = (nodeLabel == null)
}