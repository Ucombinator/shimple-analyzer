package org.ucombinator.jaam.visualizer.layout;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

public class LayoutAlgorithm
{
	// This works on a graph whose vertices have been assigned a bounding box
	final static double MARGIN_PADDING = .25;
	final static double NODES_PADDING = .5;
	static HashMap<String, Double> bboxWidth;
	static HashMap<String, Double> bboxHeight;
	
	private static void initializeSizes(AbstractLayoutVertex parentVertex){
		parentVertex.setWidth(AbstractLayoutVertex.DEFAULT_WIDTH);
		parentVertex.setHeight(AbstractLayoutVertex.DEFAULT_HEIGHT);
		Iterator<AbstractLayoutVertex> it = parentVertex.getInnerGraph().getVertices().values().iterator();
		while(it.hasNext()){
			initializeSizes(it.next());
		}
	}
	
	public static void layout(AbstractLayoutVertex parentVertex) {
		bboxWidth = new HashMap<String, Double>();
		bboxHeight = new HashMap<String, Double>();
		initializeSizes(parentVertex);
		defaultLayout(parentVertex);
	}
	

	/*********************************************************************/
	/**********LAYS OUT EACH LEVEL OF THE CLUSTERED GRAPH******************/
	/*********************************************************************/
	public static void defaultLayout(AbstractLayoutVertex parentVertex){
		HierarchicalGraph graph = parentVertex.getInnerGraph();
		Collection<AbstractLayoutVertex> vertices = graph.getVertices().values();
		Iterator<AbstractLayoutVertex> it = vertices.iterator();
		while(it.hasNext())
		{
			AbstractLayoutVertex v = it.next();
			HierarchicalGraph inner_graph = v.getInnerGraph();
			if (inner_graph.getVertices().size() != 0)
			{
				//Layout the inner graphs of each node and assign width W and height H to each node
				//X and Y coordinates are RELATIVE to the parent
				if(v.isExpanded()){
					defaultLayout(v);
				}
			}
		}
		
		/*******************************************************************************************/
		/************************ACTUAL LAYOUT FOR THE CURRENT LEVEL/GRAPH**************************/
		/*******************************************************************************************/
		AbstractLayoutVertex root = graph.getRoot();
		Iterator<AbstractLayoutVertex> itClear = graph.getVertices().values().iterator();
		// Initialize all the nodes to be WHITE
			while(itClear.hasNext()){itClear.next().setVertexStatus(AbstractLayoutVertex.VertexStatus.WHITE);}
		if(root!=null){
			storeBBoxWidthAndHeight(root);
		}
		itClear = graph.getVertices().values().iterator();
			while(itClear.hasNext()){itClear.next().setVertexStatus(AbstractLayoutVertex.VertexStatus.WHITE);}
		if(root!=null){
			assingXandYtoInnerNodesAndGiveParentBBox(root, MARGIN_PADDING, MARGIN_PADDING);
			parentVertex.setWidth(bboxWidth.get(root.getStrID()) + 2 * MARGIN_PADDING);
			parentVertex.setHeight(bboxHeight.get(root.getStrID()) + 2 * MARGIN_PADDING);
		}else{
			parentVertex.setWidth(AbstractLayoutVertex.DEFAULT_WIDTH);
			parentVertex.setHeight(AbstractLayoutVertex.DEFAULT_HEIGHT);
		}
		
	}
	
	/**
	 * Preconditions: Height and Width of the inner nodes of the graph is (resursively known)
	 * input: graph and left/top offset
	 * Changes of Status: assigns X and Y to the inner vertices of the graph
	 * Output: returns the W and H to be assign to the parent node
	 * */
	public static void assingXandYtoInnerNodesAndGiveParentBBox(AbstractLayoutVertex root, double left, double top)
	{
		root.setVertexStatus(AbstractLayoutVertex.VertexStatus.GRAY);
		Iterator<AbstractLayoutVertex> it = root.getOutgoingNeighbors().iterator();
		ArrayList<AbstractLayoutVertex> grayChildren = new ArrayList<AbstractLayoutVertex>(); 
		while(it.hasNext())
		{
			AbstractLayoutVertex child = it.next();
			if (child.getVertexStatus() == AbstractLayoutVertex.VertexStatus.WHITE)
			{
				child.setVertexStatus(AbstractLayoutVertex.VertexStatus.GRAY);
				grayChildren.add(child);
			}
		}
		
		double currentWidth = 0;
		double currentHeight = 0; 
		
		Iterator<AbstractLayoutVertex> itGray = grayChildren.iterator();
		while(itGray.hasNext())
		{
			AbstractLayoutVertex curVer = itGray.next();
			currentWidth += bboxWidth.get(curVer.getStrID()) + NODES_PADDING;
		}
		
		
		double AX;
		if(root.getWidth() >= currentWidth-NODES_PADDING){
			AX = (root.getWidth()-(currentWidth-NODES_PADDING))/2;
		}else{
			AX = 0;
		}
		
		currentWidth = 0;
		itGray = grayChildren.iterator();
		while(itGray.hasNext())
		{
			AbstractLayoutVertex curVer = itGray.next();
			assingXandYtoInnerNodesAndGiveParentBBox(curVer,currentWidth + left+AX,NODES_PADDING + top + root.getHeight());
			currentWidth += bboxWidth.get(curVer.getStrID()) + NODES_PADDING;
			currentHeight = Math.max(currentHeight, bboxHeight.get(curVer.getStrID()));
		}
		
		
		
//		if(root.getWidth() <= currentWidth){
//			root.setX(left + ((bboxWidth.get(root.getStrID()) - root.getWidth()) / 2.0));  //left-most corner x
//		}else{
//			
//		}
		root.setX(left + ((bboxWidth.get(root.getStrID()) - root.getWidth()) / 2.0));  //left-most corner x
		root.setY(top);							    					//top-most corner y
		
		root.setVertexStatus(AbstractLayoutVertex.VertexStatus.BLACK);
	}
	
	/**
	 * Preconditions: Height and Width of the inner nodes of the graph is (resursively known)
	 * input: graph and left/top offset
	 * Changes of Status: assigns X and Y to the inner vertices of the graph
	 * Output: returns the W and H to be assign to the parent node
	 * */
	public static double[] storeBBoxWidthAndHeight(AbstractLayoutVertex root)
	{
		root.setVertexStatus(AbstractLayoutVertex.VertexStatus.GRAY);
		Iterator<AbstractLayoutVertex> it = root.getOutgoingNeighbors().iterator();
		ArrayList<AbstractLayoutVertex> grayChildren = new ArrayList<AbstractLayoutVertex>(); 
		while(it.hasNext())
		{
			AbstractLayoutVertex child = it.next();
			if (child.getVertexStatus() == AbstractLayoutVertex.VertexStatus.WHITE)
			{
				child.setVertexStatus(AbstractLayoutVertex.VertexStatus.GRAY);
				grayChildren.add(child);
			}
		}
		
		double currentWidth = 0;
		double currentHeight = 0; 
		Iterator<AbstractLayoutVertex> itGray = grayChildren.iterator();
		while(itGray.hasNext())
		{
			AbstractLayoutVertex curVer = itGray.next();
			double[] boundBox = storeBBoxWidthAndHeight(curVer);
			currentWidth += boundBox[0] + NODES_PADDING;
			currentHeight = Math.max(currentHeight, boundBox[1]);
		}
		
		double bboxWIDTH, bboxHEIGHT = 0.0;
		
		bboxWIDTH = Math.max(root.getWidth(), currentWidth - NODES_PADDING);
		if(grayChildren.size() == 0)
		{
			bboxHEIGHT = root.getHeight();
		}
		else
		{
			bboxHEIGHT = NODES_PADDING + root.getHeight() + currentHeight;
		}
		
		root.setVertexStatus(AbstractLayoutVertex.VertexStatus.BLACK);
		
		bboxWidth.put(root.getStrID(), bboxWIDTH);
		bboxHeight.put(root.getStrID(), bboxHEIGHT);
		
		double[] result = {bboxWIDTH,bboxHEIGHT};
		return result;
	}
	
}


