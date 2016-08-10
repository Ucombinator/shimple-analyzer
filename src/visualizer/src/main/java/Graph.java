
import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.HashMap;
import java.util.StringTokenizer;
import java.util.LinkedList;


public class Graph
{
	public int totalVertices;
	public int baseEdges;
	public ArrayList<Vertex> vertices;
	public ArrayList<MethodVertex> methodVertices;
	public ArrayList<MethodPathVertex> methodPathVertices;
    public ArrayList<String> tagsList;
    public ArrayList<Boolean> highlightedTags;
	public HashMap<String, Method> methods;
	public HashMap<String, Class> classes;
	public AbstractVertex root;
	public View currWindow;
	static int numHotkeys = 10;
	public View[] hotkeyedViews;
	private double maxH; // required for collapse method
	public int maxIndex;


	public Graph()
	{
		this.totalVertices = 0;
		this.baseEdges = 0;
		this.vertices = new ArrayList<Vertex>();
		this.methodVertices = new ArrayList<MethodVertex>();
		this.methodPathVertices = new ArrayList<MethodPathVertex>();
		this.methods = new HashMap<String, Method>();
		this.classes = new HashMap<String, Class>();
		root = new Vertex(-1,-1);
		this.maxIndex = -1;

        this.tagsList = new ArrayList<String>();
        this.highlightedTags = new ArrayList<Boolean>();
        
		currWindow = new View(true);
		hotkeyedViews = new View[numHotkeys];
		for(int i = 0; i < numHotkeys; i++)
			hotkeyedViews[i] = new View(false);
		
		this.resetZoom();
	}
	
	public void setMaxHeight(double height)
	{
		this.maxH = height;
	}
	
	public double getMaxHeight()
	{
		return maxH;
	}
	
	public void increaseZoom(double factor, double mouseX, double mouseY)
	{
		//Don't allow zooming closer than this level
		double currSize = this.currWindow.getFracArea();
		if(factor > 1 || currSize > 1.0/Main.graph.vertices.size())
		{
			//Error values mean we don't have a valid mouse location, so we zoom to the center.
			//This will most likely occur when someone decides to use the menu items.
			if(mouseX < 0 || mouseX > 1 || mouseY < 0 || mouseY > 1)
			{
				System.out.println("Zooming in...");
				double centerX = (this.currWindow.left + this.currWindow.right)/2;
				double centerY = (this.currWindow.top + this.currWindow.bottom)/2;
				this.zoomNPan(centerX, centerY, factor);
			}
			else
			{
				//Keep the current mouse location at the same relative position.
				double width = this.currWindow.right - this.currWindow.left;
				double height = this.currWindow.bottom - this.currWindow.top;
				double mouseXFrac = (mouseX - this.currWindow.left)/width;
				double mouseYFrac = (mouseY - this.currWindow.top)/height;

				double newLeft = mouseX - mouseXFrac*factor*width;
				double newRight = mouseX + (1 - mouseXFrac)*factor*width;
				double newCenterX = (newLeft + newRight)/2;

				double newTop = mouseY - mouseYFrac*factor*height;
				double newBottom = mouseY + (1 - mouseYFrac)*factor*height;
				double newCenterY = (newTop + newBottom)/2;
				this.zoomNPan(newCenterX, newCenterY, factor);
			}
		}
	}
	
	public void resetZoom()
	{
		View newWindow = new View(true);
		currWindow.setNext(newWindow);
		newWindow.setPrev(currWindow);
		currWindow = newWindow;	
	}
	
	public void loadPreviousView()
	{
		currWindow = currWindow.getPrev();
	}	
	
	public void restoreNewView()
	{
		currWindow = currWindow.getNext();
	}	
	
	public void shiftView(int hor, int ver)
	{
		View newWindow = new View(currWindow);
		newWindow.shiftView(hor, ver);
		newWindow.setPrev(currWindow);
		currWindow.setNext(newWindow);
		currWindow = newWindow;
	}
	
	public void addHotkeyedView(int digit)
	{
		hotkeyedViews[digit] = new View(currWindow);
	}

	public void loadHotkeyedView(int digit)
	{
		if(hotkeyedViews[digit].initialized)
			zoomNPan(hotkeyedViews[digit]);
	}
	
	public void zoomNPan(double centerX, double centerY, double factor)
	{
		View newWindow = new View(currWindow);
		newWindow.setPrev(currWindow);
		currWindow.setNext(newWindow);
		newWindow.zoomNPan(centerX, centerY, factor);
		currWindow = newWindow;
	}
	
	public void zoomNPan(double left, double right, double top, double bottom)
	{
		View newWindow = new View(left, right, top, bottom);
		newWindow.setPrev(currWindow);
		currWindow.setNext(newWindow);
		currWindow = newWindow;
	}
	
	public void zoomNPan(View v)
	{
		View newWindow = new View(v);
		newWindow.setPrev(currWindow);
		currWindow.setNext(newWindow);
		currWindow = newWindow;
	}

	public void selectVertices(double x1, double x2, double y1, double y2)
	{
		for(Vertex v : this.vertices)
		{
//			v.clearAllHighlights();
			v.clearAllSelect();
			if(v.isVisible && x1 < v.x && v.x < x2 && y1 < v.y && v.y < y2)
				v.addHighlight(true, false, true, true);
		}
		
		for(MethodVertex v : this.methodVertices)
		{
//			v.clearAllHighlights();
			v.clearAllSelect();
			if(v.isVisible && x1 < v.x && v.x < x2 && y1 < v.y && v.y < y2)
				v.addHighlight(true, false, true, true);
		}
		
		for(MethodPathVertex v: this.methodPathVertices)
		{
//			v.clearAllHighlights();
			v.clearAllSelect();
			if(v.isVisible && x1 < v.x && v.x < x2 && y1 < v.y && v.y < y2)
				v.addHighlight(true, false, true, true);
		}
        Parameters.ping();
	}
	
	public AbstractVertex getVertexNearestCoordinate(double x, double y)
	{
		AbstractVertex closest = null;
		double closestDist = Double.MAX_VALUE;
		
		for(Vertex v : vertices)
		{
			double newDist = v.distTo(x,  y);
			if(v.isVisible && newDist < closestDist)
			{
				closest = v;
				closestDist = newDist;
			}
		}
		
		for(MethodVertex v : methodVertices)
		{
			double newDist = v.distTo(x,  y);
			if(v.isVisible && newDist < closestDist)
			{
				closest = v;
				closestDist = newDist;
			}				
		}
		
		for(MethodPathVertex v : methodPathVertices)
		{
			double newDist = v.distTo(x,  y);
			if(v.isVisible && newDist < closestDist)
			{
				closest = v;
				closestDist = newDist;
			}
		}
		
		return closest;
	}

	public void addErrorState(int id)
	{
		this.addVertex(id, "ErrorState", "", "ErrorState", -1, false);
	}
	
	public void addVertex(int v, String methodName, String inst, String desc, int ind, boolean drawEdges)
	{
		Vertex ver = this.containsVertex(v);
		
		if(ver == null)
		{
			totalVertices++;
			ver = new Vertex(v, this.vertices.size());
			this.vertices.add(ver);
		}
		
		this.matchVertexToMethod(ver, methodName);
		ver.setDescription(desc);
		ver.setInstruction(inst);
		ver.setNameToInstruction();
		ver.jimpleIndex = ind;
		ver.drawEdges = drawEdges;
		
		if(ind > this.maxIndex)
			this.maxIndex = ind;
	}

	public void matchVertexToMethod(Vertex v, String methodName)
	{
		Method currMethod;
		if(this.methods.containsKey(methodName))
		{
			currMethod = this.methods.get(methodName);
		}
		else
		{
			currMethod = new Method(methodName);
			this.methods.put(methodName, currMethod);
		}
		v.setMethod(currMethod);
		currMethod.addVertex(v);
	}
	
	public void addEdge(String line)
	{
		int src, dest;
		StringTokenizer token = new StringTokenizer(line);
		
		if(token.countTokens() == 3)
		{
			baseEdges++;
			src = Integer.parseInt(token.nextToken());
			token.nextToken();
			dest = Integer.parseInt(token.nextToken());

			addEdge(src, dest);
		}
	}

	public void addEdge(int src, int dest)
	{
		Vertex vSrc, vDest;

		if(src != dest)
		{
			vSrc = this.containsVertex(src);
			if (vSrc == null)
			{
				totalVertices++;
				vSrc = new Vertex(src, this.vertices.size());
				this.vertices.add(vSrc);
			}

			vDest = this.containsVertex(dest);
			if (vDest == null)
			{
				totalVertices++;
				vDest = new Vertex(dest, this.vertices.size());
				this.vertices.add(vDest);
			}

			baseEdges++;
			vSrc.addNeighbor(vDest);
		}
	}

	public void matchClassesToCode(String basePath, ArrayList<File> javaFiles)
	{
		//Search through all the files, removing basePath from the beginning and .java from the end
		for(File file : javaFiles)
		{
			String path = file.getAbsolutePath();
			String className = path.substring(basePath.length(), path.length() - 5).replace('/', '.');

			if(this.classes.containsKey(className))
				this.classes.get(className).parseJavaFile(file.getAbsolutePath());
			else
				System.out.println("Cannot find class: " + className);
		}
	}
    
    
    public void addTag(int nodeId, String tag)
    {
        Vertex ver = this.containsVertex(nodeId);
        if(ver==null)
            return;
        
        int t = this.tagPresent(tag);
        
        if(t<0)
        {
            t = this.tagsList.size();
            this.tagsList.add(tag);
            this.highlightedTags.add(new Boolean(true));
        }
        ver.addTag(t);
    }
    
    
    public int tagPresent(String tag)
    {
        int i=0;
        for(String t : this.tagsList)
        {
            if(t.equalsIgnoreCase(tag))
                return i;
            i++;
        }
        return -1;
    }
    

	public void clearHighlights()
	{
		for(Vertex v : this.vertices)
			v.clearAllHighlights();
		
		for(MethodVertex v : this.methodVertices)
			v.clearAllHighlights();

		for(MethodPathVertex v : this.methodPathVertices)
			v.clearAllHighlights();
	}
	
	public void clearSelects()
	{
		for(Vertex v : this.vertices)
			v.clearAllSelect();
		
		for(MethodVertex v : this.methodVertices)
			v.clearAllSelect();

		for(MethodPathVertex v : this.methodPathVertices)
			v.clearAllSelect();
	}
	
	public HashSet<Method> collectHighlightedMethods()
	{
		HashSet<Method> highlightedMethods = new HashSet<Method>();
		
		for(Vertex v : this.vertices)
		{
            if(v.isHighlighted || v.isSelected)
//			if(v.isHighlighted)
				highlightedMethods.add(v.getMethod());
		}
		
		for(MethodVertex v : this.methodVertices)
		{
            if(v.isHighlighted || v.isSelected)
//			if(v.isHighlighted)
				highlightedMethods.add(v.getMethod());
		}
		
		for(MethodPathVertex v : this.methodPathVertices)
		{
            if(v.isHighlighted || v.isSelected)
//			if(v.isHighlighted)
			{
				for(MethodVertex w : v.getMergeChildren())
					highlightedMethods.add(w.getMethod());
			}
		}
		
		return highlightedMethods;
	}
	
	//After a vertex has been unhighlighted, we may have to remove some highlights.
	//But that's complicated, so it's simpler (albeit slightly inefficient) to erase
	//and redo everything.
	//TODO: Make this more efficient. Right now it could be O(n^2).
	public void redoCycleHighlights()
	{
		//First we remove all cycle highlights
		for(Vertex v : Main.graph.vertices)
			v.clearCycleHighlights();
		
		//Then we rehighlight the correct cycles
		for(Vertex v : Main.graph.vertices)
		{
			if(v.isHighlighted())
			{
				//System.out.println("Highlighting cycle for vertex " + v.getFullName());
				v.highlightCycles();
			}
		}
	}
	
	public void searchNodes(StacFrame.searchType search, String searchStr)
	{
		this.clearHighlights();
		Parameters.leftArea.clear();
		
		if(search == StacFrame.searchType.ID || search == StacFrame.searchType.OUT_OPEN || search == StacFrame.searchType.OUT_CLOSED
				|| search == StacFrame.searchType.IN_OPEN || search == StacFrame.searchType.IN_CLOSED || search == StacFrame.searchType.ROOT_PATH)
		{
			StringTokenizer token = new StringTokenizer(searchStr,", ");
			
			int id1, id2;
			String tok;
			
			while(token.hasMoreTokens())
			{
				tok = token.nextToken();
				if(tok.trim().equalsIgnoreCase(""))
					continue;
				if(tok.indexOf('-')==-1)
				{
					id1 = Integer.parseInt(tok.trim());
					if(search == StacFrame.searchType.ID)
						this.searchByID(id1);
					else if(search == StacFrame.searchType.OUT_OPEN || search == StacFrame.searchType.OUT_CLOSED)
						this.searchNeighbors(id1);
					else if(search == StacFrame.searchType.IN_OPEN || search == StacFrame.searchType.IN_CLOSED)
						this.searchIncomings(id1);
					else if(search == StacFrame.searchType.ROOT_PATH)
						this.searchPathToRoot(id1);
				}
				else
				{
					id1 = Integer.parseInt(tok.substring(0,tok.indexOf('-')).trim());
					id2 = Integer.parseInt(tok.substring(tok.lastIndexOf('-')+1).trim());
					if(search == StacFrame.searchType.ID)
						this.searchByID(id1, id2);
					else if(search == StacFrame.searchType.OUT_OPEN || search == StacFrame.searchType.OUT_CLOSED)
						this.searchNeighbors(id1, id2);
					else if(search == StacFrame.searchType.IN_OPEN || search == StacFrame.searchType.IN_CLOSED)
						this.searchIncomings(id1, id2);
					else if(search == StacFrame.searchType.ROOT_PATH)
						this.searchPathToRoot(id1, id2);
				}
			}
			
		}
        else if(search == StacFrame.searchType.TAG)
        {
            this.searchByTag(0);
        }
		else if(search == StacFrame.searchType.INSTRUCTION)
		{
			this.searchByInst(searchStr);
		}
		else if(search == StacFrame.searchType.METHOD)
		{
			this.searchByMethod(searchStr);
		}
		else if(search == StacFrame.searchType.ALL_LEAVES)
		{
			//System.out.println("Searching for leaves");
			searchLeaves();
		}
		else if(search == StacFrame.searchType.ALL_SOURCES)
		{
			//System.out.println("Searching for sources");
			searchSources();
		}
		
		//this.printHighlightTo();
		//this.printHighlightFrom();
	}
	
	public void printHighlightIncoming()
	{
		System.out.print("HighlightTo Nodes: ");
		for(int i=0; i<this.vertices.size(); i++)
			if(this.vertices.get(i).isIncomingHighlighted)
				System.out.print(this.vertices.get(i).getName()+", ");
		System.out.println("");
	}
	
	public void printHighlightOutgoing()
	{
		System.out.print("HighlightFrom Nodes: ");
		for(int i=0; i<this.vertices.size(); i++)
			if(this.vertices.get(i).isOutgoingHighlighted)
				System.out.print(this.vertices.get(i).getName()+", ");
		System.out.println("");
	}
	
	//Highlight vertices with no outgoing edges
	public void searchLeaves()
	{
		for(Vertex v : this.vertices)
		{
			//System.out.println(v.neighbors.size());
			if(v.neighbors.size() == 0)
				v.addHighlight(false, true, false, false);
		}
	}

	//Highlight vertices with no incoming edges
	public void searchSources()
	{
		for(Vertex v : this.vertices)
		{
			//System.out.println(v.incoming.size());
			if(v.incoming.size() == 0)
				v.addHighlight(false, true, false, false);
		}
	}
	
	//Highlight all neighbors of the vertex with the given id
	public void searchNeighbors(int id)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id == id)
			{
				v.addHighlight(false, true, false, true);
				for(AbstractVertex w : v.neighbors)
					w.addHighlight(false, true, true, false);
			}
		}
	}
	
	//Highlight all neighbors of vertices with an id in the given range
	public void searchNeighbors(int startID, int endID)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id >= startID && v.id <= endID)
			{
				v.addHighlight(false, true, false, true);
				for(AbstractVertex w : v.neighbors)
					w.addHighlight(false, true, true, false);
			}
		}
	}
	
	//Highlight all incoming vertices to the vertex with the given id
	public void searchIncomings(int id)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id == id)
			{
				v.addHighlight(false, true, false, true);
				for(AbstractVertex w : v.incoming)
					w.addHighlight(false, true, true, false);
			}
		}
	}
	
	//Highlight all incoming vertices to a vertex with an id in the given range
	public void searchIncomings(int startID, int endID)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id >= startID && v.id <= endID)
			{
				v.addHighlight(false, true, false, true);
				for(AbstractVertex w : v.incoming)
					w.addHighlight(false, true, true, false);
			}
		}
	}
	
	//Highlight the path from the vertex with the given id to the root of the tree
	public void searchPathToRoot(int id)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id == id)
			{
				AbstractVertex ver = v;
				while(ver != this.root)
				{
					ver.addHighlight(false, true, true, true);
					ver = ver.parent;
				}
			}
		}
	}
	
	//Highlight the paths from vertices with an id in the given range to the root of the true
	public void searchPathToRoot(int startID, int endID)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id >= startID && v.id <= endID)
			{
				AbstractVertex ver = v;
				while(ver != this.root)
				{
					ver.addHighlight(false, true, true, true);
					ver = ver.parent;
				}
			}
		}
	}
	
	//Highlight vertex with the given id
	public void searchByID(int id)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id == id)
				v.addHighlight(false, true, true, true);
		}
	}
	
	//Highlight vertices with an id in the given range
	public void searchByID(int startID, int endID)
	{
		for(Vertex v : this.vertices)
		{
			if(v.id >= startID && v.id <= endID)
				v.addHighlight(false, true, true, true);
		}
	}
	
    //Highlight vertices with a given tag
    public void searchByTag(int t)
    {
        for(Vertex v : this.vertices)
        {
            if(v.hasTag(t))
                v.addHighlight(false, true, true, true);
        }
    }
    
	//Highlight vertices whose instruction contains the given search string
	public void searchByInst(String match)
	{
		for(Vertex v : this.vertices)
		{
			if(v.getInstruction().contains(match))
				v.addHighlight(false, true, true, true);
		}
	}
	
	//Highlight vertices that are in the given method
	public void searchByMethod(String match)
	{
		for(Vertex v : this.vertices)
		{
			if(v.getMethodName().contains(match))
				v.addHighlight(false, true, true, true);
		}
	}
	
	public Vertex containsVertex(int id)
	{
		for(int i=0; i<this.vertices.size(); i++)
		{
			if(this.vertices.get(i).id==id)
				return this.vertices.get(i);
		}
		return null;
	}	
	
	//Computes the instruction lists for each method
	public void computeInstLists()
	{
		for(Entry<String, Method> entry : this.methods.entrySet())
		{
			Method method = entry.getValue();
			method.collectAndSortInstructions();
		}
	}
	
	public void mergeAllByMethod()
	{
		for(AbstractVertex v : this.root.children)
		{
			if(v.vertexType == AbstractVertex.VertexType.LINE)
			{
				Vertex w = (Vertex) v;
				w.mergeByMethod(null);
			}
		}
	}
	
	public void mergePaths()
	{
		for(MethodVertex v : this.methodVertices)
			v.mergePath();
		
		for(MethodPathVertex v : this.methodPathVertices)
			v.collectMethodsAndInstructions();
	}
    
    
    public void collectAllTags()
    {
        for(MethodVertex v : this.methodVertices)
            v.collectAllTagsFromChildren();
        
        for(MethodPathVertex v : this.methodPathVertices)
            v.collectAllTagsFromChildren();
    }

	public void finalizeParentsForRootChildren()
	{
		AbstractVertex dest, src;
		boolean flag;
		for(int i = 0; i < this.root.children.size(); i++)
		{
			dest = this.root.children.get(i);
			for(int j = 0; j < dest.incoming.size(); j++)
			{
				src = dest.incoming.get(j);
				
				flag = false;
				AbstractVertex ver = src;
				while(ver != Main.graph.root)
				{
					if(ver == dest)
					{
						flag = true;
						break;
					}
					ver = ver.parent;
				}
				
				if(!flag) // if there is loop then, try to find potential point to break the loop 
				{
					dest.changeParent(src);
					i--;
					break;
				}
			}
		}
	}

	//Collapse all visible vertices once.
	//We check method path vertices and then method vertices,
	//so that we don't check the same vertex twice
	public void collapseOnce()
	{
		for(MethodPathVertex v : this.methodPathVertices)
		{
			if(v.mergeRoot.isVisible)
				v.collapse();
		}
		
		for(MethodVertex v : this.methodVertices)
		{
			if(v.mergeRoot.isVisible)
				v.collapse();
		}
	}

	//Expand all visible vertices once.
	//We must check method vertices and then method path vertices,
	//so that no vertex is expanded twice.
	public void deCollapseOnce()
	{
		for(MethodVertex v : this.methodVertices)
		{
			if(v.isVisible)
			{
				v.deCollapse();
			}
		}
		
		for(MethodPathVertex v : this.methodPathVertices)
		{
			if(v.isVisible)
			{
				v.deCollapse();
			}
		}
	}
	
	public void collapseAll()
	{
		boolean flag = true;
		while(flag)
		{
			flag = false;
			for(MethodVertex v : this.methodVertices)
			{
				if(v.mergeRoot.isVisible)
				{
					flag = true;
					v.collapse();
				}
			}
			
			for(MethodPathVertex v : this.methodPathVertices)
			{
				if(v.mergeRoot.isVisible)
				{
					flag = true;
					v.collapse();
				}
			}
		}
	}
	
	public void deCollapseAll()
	{
		boolean flag = true;
		while(flag)
		{
			flag = false;
			for(MethodVertex v : this.methodVertices)
			{
				if(v.isVisible)
				{
					flag = true;
					v.deCollapse();
				}
			}
			
			for(MethodPathVertex v : this.methodPathVertices)
			{
				if(v.isVisible)
				{
					flag = true;
					v.deCollapse();
				}
			}
		}
	}
	
	public void setAllMethodHeight()
	{
		for(MethodVertex v : this.methodVertices)
			v.setLoopHeight();
		
		for(MethodPathVertex v : this.methodPathVertices)
			v.setLoopHeight();
	}
	
	public void printAdjacencyList()
	{
		for(int i=0; i<this.vertices.size(); i++)
		{
			Vertex v = this.vertices.get(i);
			
			System.out.print(v.id+": ");
			int deg = v.getOutDegree();
			
			for(int j = 0; j < deg; j++)
			{
				System.out.print(v.neighbors.get(j).id+" ");
			}
			System.out.println("");
		}
	}
	
	public void printAllChildren()
	{
		for(int i = 0; i < this.vertices.size(); i++)
		{
			Vertex v = this.vertices.get(i);
			
			System.out.print(v.getName()+": ");
			
			for(int j=0; j<v.children.size(); j++)
			{
				System.out.print(v.children.get(j).getName()+" ");
			}
			System.out.println("");
		}
	}
	
	public void printChildren(AbstractVertex v)
	{
		System.out.print(v.getName() + ": ");
		
		for(int j=0; j<v.children.size(); j++)
		{
			System.out.print(v.children.get(j).getName() + " ");
		}
		System.out.println("");
	}
	
	public void printAllParents()
	{
		for(Vertex v: this.vertices)
			System.out.println(v.getName() + ": " + v.parent.getName());
	}
	
	public void printParent(AbstractVertex v)
	{
		System.out.println(v.getName() + ": " + v.parent.getName());
	}
	
	public void printCoordinates()
	{
		AbstractVertex v = this.root;
		System.out.println("root: " + v.x + ", " + v.y + ", left = " + v.left + ", right = " + v.right + ", width = " + v.width + ", top = " + v.top + ", bottom = " + v.bottom + ", height = " + v.height);
		for(int i=0; i<this.vertices.size(); i++)
		{
			v = this.vertices.get(i);
			
			System.out.println(v.getName() + ": " + v.x + ", " + v.y + ", left = " + v.left + ", right = " + v.right + ", width = " + v.width + ", top = " + v.top + ", bottom = " + v.bottom + ", height = " + v.height);
		}
	}

	//Return the width of our graph in box units
	public double getWidth()
	{
		return root.width;
	}

	//Return the height of our graph in box units
	public double getHeight()
	{
		return root.height - 1;
	}
	
	//Next three methods modified from "A New Algorithm for Identifying Loops in Decompilation"
	public void identifyLoops()
	{
		//Each vertex is already initialized
		for(Vertex v : vertices)
		{
			if(!v.traversed)
				travLoopsDFS(v, 1);
		}
		
		/*for(Vertex v : vertices)
		{
			Vertex header = v.getLoopHeader();
			if(header != null)
				System.out.println(v.id + " --> " + v.getLoopHeader().id);
		}*/
	}
	
	public Vertex travLoopsDFS(Vertex v0, int dfsPathPos)
	{
		//System.out.println("Expanding vertex: " + Integer.toString(v0.id));
		v0.traversed = true;
		v0.dfsPathPos = dfsPathPos;
		for(Vertex ver : v0.neighbors)
		{
			Vertex v = ver;
			//System.out.println("New child: " + Integer.toString(v.id));
			if(!v.traversed)
			{
				//Case A: v is not yet traversed
				Vertex header = travLoopsDFS(v, dfsPathPos + 1);
				tagLoopHeader(v0, header);
			}
			else
			{
				if(v.dfsPathPos > 0)
				{
					//Case B: Mark b as a loop header
					tagLoopHeader(v0, v);
				}
				else if(v.getLoopHeader() == null)
				{
					//Case C: Do nothing
				}
				else
				{
					Vertex header = v.getLoopHeader();
					if(header.dfsPathPos > 0)
					{
						//Case D
						tagLoopHeader(v0, header);
					}
					else
					{
						//Case E: Re-entry
						while(header.getLoopHeader() != null)
						{
							header = header.getLoopHeader();
							if(header.dfsPathPos > 0)
							{
								tagLoopHeader(v0, header);
								break;
							}
						}
					}
				}
			}
		}
		
		v0.dfsPathPos = 0;
		return v0.getLoopHeader();
	}
	
	public void tagLoopHeader(Vertex v, Vertex header)
	{
		if(v == header || header == null)
			return;
		
		Vertex cur1 = v;
		Vertex cur2 = header;
		while(cur1.getLoopHeader() != null)
		{
			Vertex newHeader = cur1.getLoopHeader();
			if(newHeader == cur2)
				return;
			
			if(newHeader.dfsPathPos < cur2.dfsPathPos)
			{
				cur1.setLoopHeader(cur2);
				cur1 = cur2;
				cur2 = newHeader;
			}
			else
				cur1 = newHeader;
		}
		cur1.setLoopHeader(cur2);
	}
	
	public void calcLoopHeights()
	{
		//The loop height is -1 if it has not yet been calculated.
		//We do a breadth-first search of the graph, since the vertices might not be in order in our list.

		//We begin our search from the vertices that do not have a loop header.
		ArrayList<Vertex> toSearch = new ArrayList<Vertex>();
		ArrayList<Vertex> newSearch = new ArrayList<Vertex>();
		for(Vertex v: vertices)
		{
			Vertex header = v.getLoopHeader();
			if(header == null)
			{
				v.loopHeight = 0;
				toSearch.add(v);
			}
			else
			{
				header.addLoopChild(v);
			}
		}
		
		//This loop should terminate because every vertex has exactly one loop header, and there should not
		//be a loop in following header pointers. Each pass sets the height for the vertices at the next
		//level.
		int currLoopHeight = 1;
		while(toSearch.size() > 0)
		{
			for(Vertex v : toSearch)
			{
				ArrayList<Vertex> loopChildren = v.getLoopChildren();
				if(loopChildren.size() > 0)
				{
					v.loopHeight = currLoopHeight;
					for(Vertex w : loopChildren)
						newSearch.add(w);
				}
				else
					v.loopHeight = currLoopHeight - 1;
			}
			
			toSearch = newSearch;
			newSearch = new ArrayList<Vertex>();
			currLoopHeight++;
		}
		
		System.out.println("Loop heights found!");
		VizPanel.computeHues();
	}
}
