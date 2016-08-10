
import java.io.File;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.FlowLayout;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseWheelEvent;
import java.awt.Font;

import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.BorderFactory;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import javax.swing.JTextArea;
import javax.swing.JComboBox;
import javax.swing.KeyStroke;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.text.DefaultCaret;


/**
 * JFrame showing a map
 * @author Jawaherul
 *
 */

public class StacFrame extends JFrame
{
	private JMenuBar menuBar;
	private JMenu menuFile, menuSearch, menuNavigation, menuCustomize, menuHelp;
	private int width, height;
	private JSplitPane centerSplitPane;
	public VizPanel vizPanel, contextPanel;
	private JPanel menuPanel, leftPanel, rightPanel, searchPanel;
	public JCheckBox showContext, showEdge;
	private boolean context = false, mouseDrag = false;
	
	public enum searchType
	{
		ID, TAG, INSTRUCTION, METHOD, ALL_LEAVES, ALL_SOURCES, OUT_OPEN, OUT_CLOSED, IN_OPEN, IN_CLOSED, ROOT_PATH
	}
	
	public StacFrame()
	{
		super("STAC Visualization");
		width = Parameters.width;
		height = Parameters.height;
		this.setLocation(0, 0);
		
		setSize(this.width,this.height);

		makeMenuBar();
		makeLayout();
		
		this.setVisible(true);
	}
	
	public void makeMenuBar()
	{
		menuBar = new JMenuBar();
		
		//File menu
		menuFile = new JMenu("File");
		menuBar.add(menuFile);
		/*JMenuItem loadGraph = new JMenuItem("Load graph from JSON"); //Will be removed soon
		menuFile.add(loadGraph);
		loadGraph.addActionListener(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						loadGraph(false);
					}
				}
		);*/

		JMenuItem loadMessages = new JMenuItem("Load graph from message file");
		menuFile.add(loadMessages);
		loadMessages.addActionListener(
				new ActionListener()
				{
					@Override
					public void actionPerformed(ActionEvent e)
					{
						loadGraph(true);
					}
				}
		);
        loadMessages.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, 0));

		/*final JMenuItem loadJavaCode = new JMenuItem("Load matching decompiled code");
		menuFile.add(loadJavaCode);
		loadJavaCode.addActionListener(
				new ActionListener()
				{
					@Override
					public void actionPerformed(ActionEvent e)
					{
						TakeInput.loadDecompiledCode();
					}
				}
		);*/
		
		//Search menu
		menuSearch = new JMenu("Search");
		menuBar.add(menuSearch);
		JMenuItem searchByID = new JMenuItem("by ID");
		menuSearch.add(searchByID);
		searchByID.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.ID);
						Parameters.repaintAll();
					}
				}
		);

		JMenuItem searchByInst = new JMenuItem("by Statement");
		menuSearch.add(searchByInst);
		searchByInst.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.INSTRUCTION);
						Parameters.repaintAll();
					}
				}
		);

		JMenuItem searchByMethod = new JMenuItem("by Method");
		menuSearch.add(searchByMethod);
		searchByMethod.addActionListener
		(
            new ActionListener()
            {
                public void actionPerformed(ActionEvent ev)
                {
                    searchAndHighlight(searchType.METHOD);
                    Parameters.repaintAll();
                }
            }
		);

        JMenuItem searchTags = new JMenuItem("Allocation Tags");
        menuSearch.add(searchTags);
        searchTags.addActionListener
        (
            new ActionListener()
            {
                public void actionPerformed(ActionEvent ev)
                {
                    searchAndHighlight(searchType.TAG);
                    Parameters.repaintAll();
                }
            }
        );
        searchTags.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T, 0));
        
        
		JMenuItem searchLeaves = new JMenuItem("All leaves");
		menuSearch.add(searchLeaves);
		searchLeaves.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.ALL_LEAVES);
						Parameters.repaintAll();
					}
				}
		);

		
		JMenuItem searchSources = new JMenuItem("All sources");
		menuSearch.add(searchSources);
		searchSources.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.ALL_SOURCES);
						Parameters.repaintAll();
					}
				}
		);

		
		JMenuItem searchOutgoing = new JMenuItem("Outgoing neighborhood (open)");
		menuSearch.add(searchOutgoing);
		searchOutgoing.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.OUT_OPEN);
						Parameters.repaintAll();
					}
				}
		);

		
		JMenuItem searchOutgoingclosed = new JMenuItem("Outgoing neighborhood (closed)");
		menuSearch.add(searchOutgoingclosed);
		searchOutgoingclosed.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.OUT_CLOSED);
						Parameters.repaintAll();
					}
				}
		);

		
		JMenuItem searchIncoming = new JMenuItem("Incoming neighborhood (open)");
		menuSearch.add(searchIncoming);
		searchIncoming.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.IN_OPEN);
						Parameters.repaintAll();
					}
				}
		);

		
		JMenuItem searchIncomingclosed = new JMenuItem("Incoming neighborhood (closed)");
		menuSearch.add(searchIncomingclosed);
		searchIncomingclosed.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.IN_CLOSED);
						Parameters.repaintAll();
					}
				}
		);

		JMenuItem searchPathToRoot = new JMenuItem("Path to Root");
		menuSearch.add(searchPathToRoot);
		searchPathToRoot.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						searchAndHighlight(searchType.ROOT_PATH);
						Parameters.repaintAll();
					}
				}
		);

		
		JMenuItem clearAll = new JMenuItem("Clear All");
		menuSearch.add(clearAll);
		clearAll.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.clearHighlights();
						Parameters.leftArea.clear();
						Parameters.repaintAll();
					}
				}
		);
		

		//Navigation menu
		menuNavigation = new JMenu("Navigation");
		menuBar.add(menuNavigation);
		JMenuItem zoomIn = new JMenuItem("Zoom in        (Mouse wheel up)");
		menuNavigation.add(zoomIn);
		zoomIn.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.increaseZoom(Parameters.zoomFactor, -1, -1);
						Parameters.repaintAll();
					}
				}
		);
    
		JMenuItem zoomOut = new JMenuItem("Zoom out      (Mouse wheel down)");
		menuNavigation.add(zoomOut);
		zoomOut.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.increaseZoom(1/Parameters.zoomFactor, -1, -1);
						Parameters.repaintAll();
					}
				}
		);
		
		JMenuItem resetGraph = new JMenuItem("Reset view");
		menuNavigation.add(resetGraph);
		resetGraph.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.resetZoom();
						Parameters.repaintAll();
					}
				}
		);
		resetGraph.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, 0));
		
		JMenuItem collapse = new JMenuItem("Collapse nodes");
		menuNavigation.add(collapse);
		collapse.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.collapseOnce();
						Parameters.repaintAll();
					}
				}
		);
		collapse.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, 0));

		JMenuItem decollapse = new JMenuItem("Expand nodes");
		menuNavigation.add(decollapse);
		decollapse.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.deCollapseOnce();
						Parameters.repaintAll();
					}
				}
		);
		decollapse.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0));
		
		JMenuItem previous = new JMenuItem("Previous view");
		menuNavigation.add(previous);
		previous.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.loadPreviousView();
						Parameters.repaintAll();
					}
				}
		);
		previous.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, 0));
		
		
		JMenuItem next = new JMenuItem("Return to next view");
		menuNavigation.add(next);
		next.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.restoreNewView();
						Parameters.repaintAll();
					}
				}
		);
		next.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, 0));
		
		JMenuItem panUp = new JMenuItem("Pan up");
		menuNavigation.add(panUp);
		panUp.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.shiftView(0, -1);
						Parameters.repaintAll();
					}
				}
		);
		panUp.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0));
		
		JMenuItem panDown = new JMenuItem("Pan down");
		menuNavigation.add(panDown);
		panDown.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.shiftView(0, 1);
						Parameters.repaintAll();
					}
				}
		);
		panDown.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0));
		
		JMenuItem panLeft = new JMenuItem("Pan left");
		menuNavigation.add(panLeft);
		panLeft.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.shiftView(-1, 0);
						Parameters.repaintAll();
					}
				}
		);
		panLeft.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0));
		
		JMenuItem panRight = new JMenuItem("Pan right");
		menuNavigation.add(panRight);
		panRight.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						Main.graph.shiftView(1, 0);
						Parameters.repaintAll();
					}
				}
		);
		panRight.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0));
		
		
		//Customize display
		menuCustomize = new JMenu("Customize");
		menuBar.add(menuCustomize);
		
		JMenuItem changeFont = new JMenuItem("Change font size");
		menuCustomize.add(changeFont);
		changeFont.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						String newFontSize = JOptionPane.showInputDialog(null, "The current font size is: " + Parameters.font.getSize() + ". Please enter a new font size");
						Parameters.font = new Font("Serif", Font.PLAIN, Integer.parseInt(newFontSize));
						Parameters.leftArea.setFont(Parameters.font);
						Parameters.rightArea.setFont(Parameters.font);
						Parameters.repaintAll();
					}
				}
		);
		changeFont.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, 0));
		
		//Help menu
		menuHelp = new JMenu("Help");
		menuBar.add(menuHelp);
		JMenuItem help = new JMenuItem("Shortcuts");
		menuHelp.add(help);
		help.addActionListener
		(
				new ActionListener()
				{
					public void actionPerformed(ActionEvent ev)
					{
						JOptionPane.showMessageDialog(getParent(), 
								"The following keyboard shortcuts are implemented.\n"
								+ "R: Reset zoom level to show entire graph \n"
								+ "C: Collapse all nodes by method \n"
								+ "D: Uncollapse all nodes \n"
								+ "P: Return to previous view \n"
								+ "N: Continue from previous to next view \n"
								+ "F: Change font size \n"
								+ "Arrows: Pan up, down, left or right \n"
								+ "Left-Click: Uncollapse a node \n"
								+ "Right-Click: Collapse all nodes of a single method \n"
								+ "Shift-click: Select/de-select multiple vertices \n"
								+ "CTRL + <Digit>: Hotkey view with all vertices currently on screen to <Digit> \n"
								+ "<Digit>: Go to hotkeyed view \n"
								+ "H: Open this list of shortcuts"
						);
					}
				}
		);
		help.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H, 0));
	}
	
	public void makeLayout()
	{
		//centerPanel and vizPanel
		this.setLayout(new BorderLayout());
		setSplitScreen();
		
		this.addMouseToViz();
		this.addKeyboard(vizPanel);
		this.addMouseToContext();
		this.addKeyboard(contextPanel);
		
        
        JPanel topPanel = new JPanel();
        topPanel.setBorder(BorderFactory.createEtchedBorder());
        topPanel.setLayout(new BorderLayout());
//        topPanel.setLayout(new GridLayout(2,1));
        this.getContentPane().add(topPanel, BorderLayout.NORTH);
        
        
        
		//menuPanel
		this.menuPanel = new JPanel();
		this.menuPanel.setBorder(BorderFactory.createEtchedBorder());
		this.menuPanel.setLayout(new FlowLayout());
//		this.getContentPane().add(this.menuPanel, BorderLayout.NORTH);
        topPanel.add(this.menuPanel, BorderLayout.CENTER);
        
		
		JPanel contextPanel = new JPanel();
		contextPanel.setBorder(BorderFactory.createEtchedBorder());
		contextPanel.setLayout(new GridLayout(1,1));
		this.menuPanel.add(contextPanel);
		
		showEdge = new JCheckBox("Show Edge");
		showEdge.setEnabled(true);
		showEdge.setSelected(true);
		showEdge.addItemListener
		(
			new ItemListener()
			{
				public void itemStateChanged(ItemEvent e)
				{
					vizPanel.showEdge = showEdge.isSelected();					
					StacFrame.this.repaint();
				}
			}
		);
		contextPanel.add(showEdge);
		
		JPanel sizePanel = new JPanel();
		sizePanel.setBorder(BorderFactory.createEtchedBorder());
		sizePanel.setLayout(new GridLayout(1,3));
		this.menuPanel.add(sizePanel);
		
		JButton sizeMinus = new JButton("-");
		sizeMinus.setEnabled(true);
		sizeMinus.addActionListener
		(
			new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					vizPanel.boxSize *= Parameters.boxFactor;
					Parameters.repaintAll();
				}
			}
		);
		sizePanel.add(sizeMinus);
		
		JLabel sizeL = new JLabel("box size");
		sizeL.setHorizontalAlignment(SwingConstants.CENTER);
		sizePanel.add(sizeL);
		
		JButton sizePlus = new JButton("+");
		sizePlus.setEnabled(true);
		sizePlus.addActionListener
		(
			new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					vizPanel.boxSize /= Parameters.boxFactor;
					Parameters.repaintAll();
				}
			}
		);
		sizePanel.add(sizePlus);
        
        
        //////********************** tag Panel ******************//////
        
/*
        JPanel tagPanel = new JPanel();
        tagPanel.setBorder(BorderFactory.createEtchedBorder());
        tagPanel.setLayout(new FlowLayout());
        topPanel.add(tagPanel, BorderLayout.SOUTH);
        
        JLabel tLab = new JLabel("Search for Tags: ");
        tagPanel.add(tLab);
        
        JComboBox combo = new JComboBox(new Object[]{"Ester", "Jordi", "Jordina", "Jorge", "Sergi"});
        combo.setEditable(true);
        tagPanel.add(combo);
 */
        
        //////******************** tag Panel end ****************//////
        
		
		
		this.setJMenuBar(menuBar);
		this.setVisible(true);
		this.repaint();
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);	
	}
	
	public void setSplitScreen()
	{
		centerSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
		JSplitPane leftSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
		JSplitPane vizSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
        JSplitPane rightSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
		centerSplitPane.setLeftComponent(leftSplitPane);
		centerSplitPane.setRightComponent(vizSplitPane);
        vizSplitPane.setRightComponent(rightSplitPane);
		this.getContentPane().add(this.centerSplitPane, BorderLayout.CENTER);
		
		centerSplitPane.setOneTouchExpandable(true);
		leftSplitPane.setOneTouchExpandable(true);
        vizSplitPane.setOneTouchExpandable(true);
		rightSplitPane.setOneTouchExpandable(true);
		
		leftPanel = new JPanel();
		leftSplitPane.setLeftComponent(leftPanel);
		rightPanel = new JPanel();
        rightSplitPane.setLeftComponent(rightPanel);
        searchPanel = new JPanel();
		rightSplitPane.setRightComponent(searchPanel);

		this.vizPanel = new VizPanel(this, false);
		vizSplitPane.setLeftComponent(this.vizPanel);
		this.contextPanel = new VizPanel(this,true);
		leftSplitPane.setRightComponent(this.contextPanel);

		JLabel leftL = new JLabel("Context", JLabel.CENTER);
		Parameters.leftArea = new CodeArea();
		leftPanel.setLayout(new BorderLayout());
		leftPanel.add(leftL,BorderLayout.NORTH);
		JScrollPane scrollL = new JScrollPane (Parameters.leftArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		leftPanel.add(scrollL, BorderLayout.CENTER);
		leftPanel.setFont(Parameters.font);
		
		JLabel rightL = new JLabel("Description", JLabel.CENTER);
		Parameters.rightArea = new JTextArea();
		Parameters.rightArea.setEditable(false);
        ((DefaultCaret)Parameters.rightArea.getCaret()).setUpdatePolicy(DefaultCaret.NEVER_UPDATE);
		rightPanel.setLayout(new BorderLayout());
		rightPanel.add(rightL, BorderLayout.NORTH);
		JScrollPane scrollR = new JScrollPane (Parameters.rightArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		rightPanel.add(scrollR, BorderLayout.CENTER);
		rightPanel.setFont(Parameters.font);
		
        JLabel searchL = new JLabel("Search Results", JLabel.CENTER);
        Parameters.searchArea = new SearchArea();
        searchPanel.setLayout(new BorderLayout());
        searchPanel.add(searchL,BorderLayout.NORTH);
        JScrollPane scrollS = new JScrollPane (Parameters.searchArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        searchPanel.add(scrollS, BorderLayout.CENTER);
        searchPanel.setFont(Parameters.font);
        
		double rw1 = 0.2, rw2 = 0.75, rw3 = 0.6, rw4 = 0.6;
		centerSplitPane.setResizeWeight(rw1);
		vizSplitPane.setResizeWeight(rw2);
		leftSplitPane.setResizeWeight(rw3);
        rightSplitPane.setResizeWeight(rw4);
		
		centerSplitPane.resetToPreferredSizes();
        vizSplitPane.resetToPreferredSizes();
		leftSplitPane.resetToPreferredSizes();
        rightSplitPane.resetToPreferredSizes();
	}
	
    
    ///modified
    public void setSplitScreenold()
    {
        //		System.out.println("Setting split screen");
        
        centerSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
        JSplitPane leftSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
        JSplitPane rightSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
        centerSplitPane.setLeftComponent(leftSplitPane);
        centerSplitPane.setRightComponent(rightSplitPane);
        this.getContentPane().add(this.centerSplitPane, BorderLayout.CENTER);
        
        centerSplitPane.setOneTouchExpandable(true);
        leftSplitPane.setOneTouchExpandable(true);
        rightSplitPane.setOneTouchExpandable(true);
        
        leftPanel = new JPanel();
        leftSplitPane.setLeftComponent(leftPanel);
        rightPanel = new JPanel();
        rightSplitPane.setRightComponent(rightPanel);
        
        this.vizPanel = new VizPanel(this, false);
        rightSplitPane.setLeftComponent(this.vizPanel);
        this.contextPanel = new VizPanel(this,true);
        leftSplitPane.setRightComponent(this.contextPanel);
        
        JLabel leftL = new JLabel("Context", JLabel.CENTER);
        Parameters.leftArea = new CodeArea();
        leftPanel.setLayout(new BorderLayout());
        leftPanel.add(leftL,BorderLayout.NORTH);
        JScrollPane scrollL = new JScrollPane (Parameters.leftArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        leftPanel.add(scrollL, BorderLayout.CENTER);
        leftPanel.setFont(Parameters.font);
        
        JLabel rightL = new JLabel("Description", JLabel.CENTER);
        Parameters.rightArea = new JTextArea();
        Parameters.rightArea.setEditable(false);
        rightPanel.setLayout(new BorderLayout());
        rightPanel.add(rightL, BorderLayout.NORTH);
        JScrollPane scrollR = new JScrollPane (Parameters.rightArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        rightPanel.add(scrollR, BorderLayout.CENTER);
        rightPanel.setFont(Parameters.font);
        
        double rw1 = 0.2, rw2 = 0.75, rw3 = 0.6;
        centerSplitPane.setResizeWeight(rw1);
        rightSplitPane.setResizeWeight(rw2);
        leftSplitPane.setResizeWeight(rw3);
        
        centerSplitPane.resetToPreferredSizes();
        rightSplitPane.resetToPreferredSizes();
        leftSplitPane.resetToPreferredSizes();
    }
    
	public void loadGraph(boolean fromMessages)
	{
		File file = Parameters.openFile(false);
		if(file == null)
			return;

		TakeInput ti = new TakeInput();
		ti.run(file.getAbsolutePath(), fromMessages);
		Parameters.repaintAll();
	}
	
	public void searchAndHighlight(searchType search)
	{
		String title = "";
		System.out.println("Search type: " + search);
		if(search == searchType.ID || search == searchType.ROOT_PATH)
		{
			title = "Enter node ID(s)";
		}
		else if(search == searchType.INSTRUCTION)
		{
			title = "Instruction contains ...";
		}
		else if(search == searchType.METHOD)
		{
			title = "Method name contains ...";
		}
		else if(search == searchType.OUT_OPEN || search == searchType.OUT_CLOSED || search == searchType.IN_OPEN || search == searchType.IN_CLOSED)
		{
			title = "Enter node ID";
		}

		String input = "";
		if(search != searchType.ALL_LEAVES && search != searchType.ALL_SOURCES && search != searchType.TAG)
		{
			input = JOptionPane.showInputDialog(null, title);
			if(input == null)
				return;
			else
				input = input.trim();

			if(input.equals(""))
				return;
		}

		Main.graph.searchNodes(search, input);
		
		Parameters.vertexHighlight = true;

		Parameters.highlightOutgoing = search != searchType.OUT_OPEN;
		Parameters.highlightIncoming = search != searchType.IN_OPEN;
    
    }
	
	public boolean isGraphLoaded()
	{
		return (Main.graph != null);
	}
	
	public void addMouseToViz()
	{
		vizPanel.addMouseListener
		(
			new MouseListener()
			{
				public void mouseClicked(MouseEvent m)
				{
					if(!isGraphLoaded())
						return;
					
					vizPanel.requestFocusInWindow();
					context = false;
					double x = vizPanel.getRelativeFracFromAbsolutePixelX(getRelativeXPixels(m))*Main.graph.getWidth();
					double y = vizPanel.getRelativeFracFromAbsolutePixelY(getRelativeYPixels(m))*Main.graph.getHeight();
					
					if(System.currentTimeMillis() - Parameters.mouseLastTime > Parameters.mouseInterval)
					{
						Parameters.mouseLastTime = System.currentTimeMillis();
						
						if(SwingUtilities.isLeftMouseButton(m) && Main.graph != null)
						{	
							if(m.isShiftDown())
							{
								AbstractVertex ver = Main.graph.getVertexNearestCoordinate(x, y);
								if(ver != null)
								{
//									if(ver.isHighlighted())
									if(ver.isSelected())
									{
//										ver.clearAllHighlights();
										ver.clearAllSelect();
										Main.graph.redoCycleHighlights();
									}
									else
									{
										ver.addHighlight(true, false, true, true);
										if(ver.vertexType == AbstractVertex.VertexType.LINE)
											((Vertex) ver).highlightCycles();
                                        Parameters.ping();
									}
								}
							}
							else if(m.getClickCount() == 1)
							{
								AbstractVertex ver = Main.graph.getVertexNearestCoordinate(x, y);
								Parameters.leftArea.clear();
                                Main.graph.clearSelects();

								if(ver != null)
								{
									Parameters.highlightIncoming = true;
									Parameters.highlightOutgoing = true;
									Parameters.vertexHighlight = true;
									ver.addHighlight(true, false, true, true);
									if(ver.vertexType == AbstractVertex.VertexType.LINE)
										((Vertex) ver).highlightCycles();
                                    Parameters.ping();
                                    Parameters.fixCaretPositions();
								}
							}
							
							else if(m.getClickCount() == 2)
							{
								AbstractVertex ver = Main.graph.getVertexNearestCoordinate(x, y);
								if(ver != null)
								{
									if(ver.getMergeChildren().size() > 0) //We have a merge vertex
									{
										ver.deCollapse();
										Parameters.repaintAll();
									}
								}
							}
						}
						
						if(SwingUtilities.isRightMouseButton(m) && Main.graph != null)
						{
							//if(m.getClickCount() == 1)
							{
								AbstractVertex ver = Main.graph.getVertexNearestCoordinate(x, y);
								if(ver != null)
								{
									if(ver.getMergeParent() != null)
									{
										ver.getMergeParent().collapse();
									}
								}
							}
						}
						
						Parameters.repaintAll();
					}
				}
				
				public void mouseEntered(MouseEvent arg0){}
				
				public void mouseExited(MouseEvent arg0){}

				public void mousePressed(MouseEvent m)
				{
					if(!isGraphLoaded())
						return;
					context = false;
					vizPanel.selectLeft = getRelativeXPixels(m);
					vizPanel.selectTop = getRelativeYPixels(m);
					vizPanel.showSelection = true;
					
					Parameters.startTime = System.currentTimeMillis();
					Parameters.lastInterval = -1;
				}

				public void mouseReleased(MouseEvent ev)
				{
					if(!isGraphLoaded())
						return;
					vizPanel.requestFocus();
					vizPanel.showSelection = false;
					context = false;
					
					if(mouseDrag)
					{
						mouseDrag = false;
						if(Main.graph != null)
						{
							double x1 = vizPanel.getIndexFromCurrentPixelX(vizPanel.selectLeft);
							double x2 = vizPanel.getIndexFromCurrentPixelX(vizPanel.selectRight);
							double y1 = vizPanel.getIndexFromCurrentPixelY(vizPanel.selectTop);
							double y2 = vizPanel.getIndexFromCurrentPixelY(vizPanel.selectBottom);

							Main.graph.selectVertices(x1, x2, y1, y2);
						}
						Parameters.repaintAll();
					}
				}
			}
		);
		
		vizPanel.addMouseMotionListener
		(
			new MouseMotionListener()
			{
				public void mouseMoved(MouseEvent m){}				

				public void mouseDragged(MouseEvent m)
				{
					if(!isGraphLoaded())
						return;
					context = false;
					mouseDrag = true;
					vizPanel.selectRight = getRelativeXPixels(m);
					vizPanel.selectBottom = getRelativeYPixels(m);

					if((System.currentTimeMillis()-Parameters.startTime)/Parameters.refreshInterval > Parameters.lastInterval)
					{
						StacFrame.this.repaint();
						Parameters.lastInterval = (System.currentTimeMillis()-Parameters.startTime)/Parameters.refreshInterval;
					}
				}
			}
		);

		vizPanel.addMouseWheelListener
		(
			new MouseWheelListener()
			{
				public void mouseWheelMoved(MouseWheelEvent e)
				{
					if(!isGraphLoaded())
						return;

					int notches = e.getWheelRotation();
					
					//Zoom in or box++ for mouse wheel up, zoom out or box-- for mouse wheel down
					if(notches > 0)
					{
						if(e.isShiftDown())
						{
							vizPanel.boxSize /= Parameters.boxFactor;
						}
						else
						{
							double x = vizPanel.getRelativeFracFromAbsolutePixelX(getRelativeXPixels(e));
							double y = vizPanel.getRelativeFracFromAbsolutePixelY(getRelativeYPixels(e));
							Main.graph.increaseZoom(Parameters.zoomFactor, x, y);
						}
					}
					else
					{
						if(e.isShiftDown())
						{
							vizPanel.boxSize *= Parameters.boxFactor;
						}
						else
						{
							double x = vizPanel.getRelativeFracFromAbsolutePixelX(getRelativeXPixels(e));
							double y = vizPanel.getRelativeFracFromAbsolutePixelY(getRelativeYPixels(e));
							Main.graph.increaseZoom(1/Parameters.zoomFactor, x, y);
						}
					}
					
					Parameters.repaintAll();
				}
			}
		);
		
	}
	
	public void addMouseToContext()
	{
		contextPanel.addMouseListener
		(
			new MouseListener()
			{
				public void mouseClicked(MouseEvent m)
				{
					if(!isGraphLoaded())
						return;
					contextPanel.requestFocusInWindow();
					context = true;
					
					double x = contextPanel.getRelativeFracFromAbsolutePixelX(getRelativeXPixels(m));
					double y = contextPanel.getRelativeFracFromAbsolutePixelY(getRelativeYPixels(m));

					Main.graph.zoomNPan(x, y, 1.0);
					Parameters.repaintAll();

					if(System.currentTimeMillis() - Parameters.mouseLastTime > Parameters.mouseInterval)
					{
						Parameters.mouseLastTime = System.currentTimeMillis();						
					}
				}
				
				public void mouseEntered(MouseEvent arg0)
				{
				}
				
				public void mouseExited(MouseEvent arg0)
				{
				}

				public void mousePressed(MouseEvent m)
				{
					if(!isGraphLoaded())
						return;
					context = true;
					contextPanel.selectLeft = getRelativeXPixels(m);
					contextPanel.selectTop = getRelativeYPixels(m);
					contextPanel.showSelection = true;
					
					Parameters.startTime = System.currentTimeMillis();
					Parameters.lastInterval = -1;
				}

				public void mouseReleased(MouseEvent ev)
				{
					if(!isGraphLoaded())
						return;
					context = true;
//					vizPanel.requestFocus();
					contextPanel.showSelection = false;
					
					if(mouseDrag)
					{
						mouseDrag = false;
						if(Main.graph != null)
						{
							double x1 = contextPanel.getRelativeFracFromAbsolutePixelX(contextPanel.selectLeft);
							double x2 = contextPanel.getRelativeFracFromAbsolutePixelX(contextPanel.selectRight);
							double y1 = contextPanel.getRelativeFracFromAbsolutePixelY(contextPanel.selectTop);
							double y2 = contextPanel.getRelativeFracFromAbsolutePixelY(contextPanel.selectBottom);
							Main.graph.zoomNPan(x1, x2, y1, y2);
						}
						Parameters.repaintAll();
					}
				}

			}

		);
		
		contextPanel.addMouseMotionListener
		(
			new MouseMotionListener()
			{
				public void mouseMoved(MouseEvent m){}
				

				public void mouseDragged(MouseEvent m)
				{
					if(!isGraphLoaded())
						return;
					context = true;
					mouseDrag = true;
					contextPanel.selectRight = getRelativeXPixels(m);
					contextPanel.selectBottom = getRelativeYPixels(m);

					if((System.currentTimeMillis()-Parameters.startTime)/Parameters.refreshInterval > Parameters.lastInterval)
					{
						StacFrame.this.repaint();
						Parameters.lastInterval = (System.currentTimeMillis()-Parameters.startTime)/Parameters.refreshInterval;
					}
				}
			}
		);

		
		contextPanel.addMouseWheelListener
		(
			new MouseWheelListener()
			{
				public void mouseWheelMoved(MouseWheelEvent e)
				{
					if(!isGraphLoaded())
						return;
					int notches = e.getWheelRotation();
					
					//box++ for mouse wheel up, box-- for mouse wheel down
					if(notches > 0)
					{
						contextPanel.boxSize /= Parameters.boxFactor;
					}
					else
					{
						contextPanel.boxSize *= Parameters.boxFactor;
					}
					Parameters.repaintAll();
				}
			}
		);

	}
	
	//Gets the x location of a mouse event in pixels, shifted so that the left side is 0.
	public double getRelativeXPixels(MouseEvent m)
	{
		if(this.context)
			return m.getX() - contextPanel.getLocation().x;
		else
			return m.getX() - vizPanel.getLocation().x - this.leftPanel.getWidth() - centerSplitPane.getDividerSize();
	}

	//Gets the y location of a mouse event in pixels, shifted so that the top of the current panel is 0.
	public double getRelativeYPixels(MouseEvent m)
	{
		//TODO: Why is this not different for the context menu?
		//Subtract the top bar, the menu panel height, and the start height of the current panel
		return m.getY() - (this.getHeight() - this.getContentPane().getSize().height) - vizPanel.getY() - this.menuPanel.getHeight();
	}

	public void addKeyboard(JPanel viz)
	{
		viz.addKeyListener(new KeyListener()
			{
				public void keyTyped(KeyEvent ev){}
				
				public void keyPressed(KeyEvent ev)
				{
					int code = ev.getKeyCode();
//					System.out.println("Key pressed: " + code);
					
					if(code == 'L')
					{
						String lim = JOptionPane.showInputDialog(null, "Set Limit on the number of vertices:");
						Parameters.limitV = Long.parseLong(lim);
						Parameters.repaintAll();
					}

					if(Character.isDigit(code))
					{
						int digit = code - '0';
						if(ev.isControlDown())
						{
							//Assign hotkey to view
							System.out.println("Assigning hotkey to " + digit);
							Main.graph.addHotkeyedView(digit);
						}
						else
						{
							//Go to hotkeyed view
							System.out.println("Loading hotkeyed view: " + digit);
							Main.graph.loadHotkeyedView(digit);
							Parameters.repaintAll();
						}
					}
				}
					
				public void keyReleased(KeyEvent ev)
				{
//					System.out.println("key released: "+ev.getKeyCode());
				}
			});
		
		viz.setFocusable(true);
		viz.requestFocusInWindow();
	}
}
