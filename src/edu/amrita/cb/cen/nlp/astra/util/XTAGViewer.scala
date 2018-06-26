package edu.amrita.cb.cen.nlp.astra.util


import javax.swing.{ JFrame, JScrollPane, JSplitPane, JList, ListSelectionModel }
import javax.swing.{ JTextArea, SwingConstants, UIManager }
import javax.swing.event.{ ListSelectionEvent, ListSelectionListener }
import java.awt.{ Font, Frame, Toolkit }
import java.io.{ File, FilenameFilter }
import java.util.NoSuchElementException
import scala.annotation.tailrec
import java.awt.Color
import javax.swing.border.Border
import javax.swing.BorderFactory
import java.awt.Insets
import javax.swing.JButton
import java.awt.BorderLayout
import javax.swing.JLabel

object XTAGViewer extends NodeSelectionListener {
	val TITLE = "ASTraM XTAG tree viewer v1.7"

	// All the following values are mutable Swing objects and hence needs to be private... 
	private val mwindow = new JFrame(XTAGViewer.TITLE)
	private val swindow = new JFrame("Tree Families")
		
	private val treeScrollPane = new JScrollPane
	private val listScrollPane = new JScrollPane
	private val familyScrollPane = new JScrollPane
	private val mainSplitPane = new JSplitPane
	private val treeSplitPane = new JSplitPane
	private val annotSplitPane = new JSplitPane

	private val unieqScrollPane = new JScrollPane
	private val unieqArea = new JTextArea
	private val commentArea = new JTextArea
	private val commentScrollPane = new JScrollPane
	
	// This is a mutable Map variable, hence needs to be private ... 
	private var fileMap = Map[String, List[TreePanel]]()
	private var curTree: Tree = null
	private var curEquv: List[String] = null

	mwindow setExtendedState Frame.MAXIMIZED_BOTH
	mwindow setUndecorated true
	mwindow setOpacity 0.7f
	mwindow setDefaultCloseOperation JFrame.EXIT_ON_CLOSE
	
	swindow setDefaultCloseOperation JFrame.EXIT_ON_CLOSE
	swindow setResizable false
			
	commentArea setEditable false 
	commentArea setLineWrap true 
	commentArea setFont new Font(TreeViewer.FONT, TreeViewer.FONTSTYLE, 20) 
	commentArea setForeground TreeViewer.FCOLOR
	commentArea setBackground TreeViewer.BCOLOR

	unieqArea setEditable false 
	unieqArea setLineWrap true 
	unieqArea setFont new Font(TreeViewer.FONT, TreeViewer.FONTSTYLE, 20) 
	unieqArea setForeground TreeViewer.FCOLOR 
	unieqArea setBackground TreeViewer.BCOLOR

	mwindow setContentPane mainSplitPane  

	mainSplitPane setLeftComponent listScrollPane 
	mainSplitPane setRightComponent treeSplitPane 
	mainSplitPane setDividerSize 1
	mainSplitPane setBorder BorderFactory.createEmptyBorder
	
	treeSplitPane setOrientation JSplitPane.VERTICAL_SPLIT 
	treeSplitPane setLeftComponent treeScrollPane 
	treeSplitPane setRightComponent annotSplitPane 
	treeSplitPane setDividerSize 1
	treeSplitPane setBorder BorderFactory.createEmptyBorder
	
	annotSplitPane setLeftComponent commentScrollPane 
	annotSplitPane setRightComponent unieqScrollPane 
	annotSplitPane setDividerSize 1
	annotSplitPane setBorder BorderFactory.createEmptyBorder
	
	commentScrollPane setViewportView commentArea 
	unieqScrollPane setViewportView unieqArea 
	treeScrollPane setBorder BorderFactory.createEmptyBorder
	commentScrollPane setBorder BorderFactory.createEmptyBorder
	unieqScrollPane setBorder BorderFactory.createEmptyBorder
	listScrollPane setBorder BorderFactory.createEmptyBorder
	
	unieqArea setMargin new Insets(10,10,1,1)
	commentArea setMargin new Insets(10,10,1,10)
	
	var panelList: List[TreePanel] = null

	def nodeSelected(evt: NodeSelectionEvent) = {
		unieqArea.setText(
			try {
				if (!evt.isNull)
					curEquv.filter(x =>
						if (evt.node.isNilSubscript) x.substring(0, x.indexOf('.')).compareTo(evt.fullNodeName) == 0
						else x.startsWith(evt.fullNodeName)  
					).reduce[String]((x, y) => x + "\n" + y)
				else curTree.annotation.unification
			} catch {
				case e: UnsupportedOperationException => "---Unification Info Missing---"
				case e: Throwable =>  "---This node has no Unification---"					
			})
	}

	private def panalListCreator(treefile: String) = {

		val treeList = Tree.readXTAGTreeFile(treefile)

		@tailrec def reccur(
				treeList: List[Tree],
				panelList: List[TreePanel]): List[TreePanel] = treeList match {
			case Nil => panelList
			case x :: ys => 
				reccur(
					ys,	{	
						val a = new TreePanel(x)
						a addNodeSelectionListener this 
						a
					} :: panelList
				)
		}
		reccur(treeList, Nil).reverse
	}

	def viewLauncher(treefile: String) = {

		try { panelList = fileMap(treefile) }
		catch {
			case e: NoSuchElementException => {
				panelList = panalListCreator(treefile)
				fileMap = fileMap + (treefile -> panelList)
			}
			case e: Throwable => println("Exception: " + e.getMessage())
		}

		if (panelList == null)
			println("Error in " + treefile)
		else {
			val list = new JList[TreePanel](panelList.toArray[TreePanel])

			list setSelectionMode ListSelectionModel.SINGLE_SELECTION 
			list setFont new Font(TreeViewer.FONT, TreeViewer.FONTSTYLE, 20) 
			
			list setForeground TreeViewer.FCOLOR 
			list setBackground TreeViewer.BCOLOR			
			list setSelectionBackground TreeViewer.SBCOLOR 
			list setSelectionForeground TreeViewer.SFCOLOR
								
			listScrollPane setViewportView list 
			
			list addListSelectionListener new ListSelectionListener {

				def valueChanged(evt: ListSelectionEvent) {
					if (!evt.getValueIsAdjusting()) {
						treeScrollPane setViewportView list.getSelectedValue 

						curTree = list.getSelectedValue.tree
						curEquv = curTree.annotation.unification.split("\n").toList

						commentArea setText curTree.comment 
						unieqArea setText curTree.annotation.unification 
					}
				}
			} 

			if (mwindow.isVisible) mwindow.validate else {
				mwindow setVisible true 
				mainSplitPane setDividerLocation 220 
				treeSplitPane setDividerLocation 600 
				annotSplitPane setDividerLocation 700
				mwindow.validate
			}
			list setSelectedIndex 0 
			treeScrollPane setViewportView panelList.head 
		}
	}

  def fileListGetter(path: String) = {

		val file = new File(path)
		if (!file.isDirectory()) null
		val treefileFilter = new FilenameFilter {

			def accept(dir: File, name: String): Boolean = {
				if (name endsWith "trees") true
				else false
			}
		}
		file listFiles treefileFilter 
	}	
	
	
	private def familyViewLauncher(treesPath: String) = {
		val treefileList = fileListGetter(treesPath)

		val treenameList = for (e <- treefileList) yield e.getName substring (0, e.getName.length - 6 )
		
		swindow setContentPane familyScrollPane
		
		val familyList = new JList[String](treenameList)

		familyList setSelectionMode ListSelectionModel.SINGLE_SELECTION 
		familyList setFont new Font(TreeViewer.FONT, TreeViewer.FONTSTYLE, 20) 
		familyList setForeground TreeViewer.FCOLOR 
		familyList setBackground TreeViewer.BCOLOR			
	  familyList setSelectionBackground TreeViewer.SBCOLOR 
	  familyList setSelectionForeground TreeViewer.SFCOLOR					
		
		familyScrollPane setViewportView familyList

		familyList.addListSelectionListener(new ListSelectionListener() {

			def valueChanged(evt: ListSelectionEvent) {
				if (!evt.getValueIsAdjusting())
					viewLauncher(treefileList(familyList.getSelectedIndex).getAbsolutePath)
			}
		})

		val maxX = Toolkit.getDefaultToolkit.getScreenSize.getWidth.asInstanceOf[Int]
				
		swindow setSize (200, 300)
		swindow pack
		
		familyList setSelectedIndex 16
		familyList ensureIndexIsVisible 20
		
		swindow setLocation (maxX - 210, 10)
		swindow setVisible true 

		viewLauncher ( treefileList(16) getAbsolutePath )

		swindow setAlwaysOnTop true 
	}

	def main(args: Array[String]) = {			
		UIManager.put("List.focusCellHighlightBorder", BorderFactory.createEmptyBorder )
		familyViewLauncher("xtag/english/grammar")
	}

}