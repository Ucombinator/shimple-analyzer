
import java.util.ArrayList;

public class Vertex extends AbstractVertex
{
	public int jimpleIndex;
	public String description = "", instruction = "";
	public MethodVertex mergeParent;
	public Method method;

	//neighbors stores the edges of the vertices in our base graph, before collapsing occurs
	protected ArrayList<Vertex> neighbors;

	//Used for shading vertices based on the number of nested loops they contain
	//loopHeight is stored for all vertices
	private Vertex loopHeader;
	private ArrayList<Vertex> loopChildren;
	public int dfsPathPos;
	public boolean traversed;
	
	public Vertex(int d, int i)
	{
		this(d, i, Integer.toString(d), d >= 0 );
	}
	
	public Vertex(int d, int i, String nm, boolean addC)
	{
		this.setDefaults();
		this.neighbors = new ArrayList<Vertex>();
		vertexType = VertexType.LINE;
		
		this.id = d;
		this.name = nm;
		this.index = i;
		
		this.parent = null;
		this.mergeParent = null;
		this.parentIndex = -1;
		
		this.isVisible = addC;
		this.numChildrenHighlighted = 0;
		this.numChildrenSelected = 0;

		if(addC)
		{
			Main.graph.root.addChild(this);
		}

		loopHeader = null;
		loopChildren = new ArrayList<Vertex>();
		loopHeight = -1;
		dfsPathPos = -1;
		traversed = false;
	}
	
	public void setMethod(Method m)
	{
		this.method = m;
	}
	
	public String getRightPanelContent()
	{
		String str = "Regular Vertex (loop height = " + this.loopHeight + ")\n"
				+ "id: " + this.id + "\n"
				+ "statement: " + this.getInstruction() + "\n"
				+ "method: " + this.getMethodName() + "\n"
				+ " location (left, right, top, bottom): "
				+ this.left + ", " + this.right + ", " + this.top + ", " + this.bottom + "\n"
				+ this.getDescription() + "\n";
		return str;
	}
	
    public String getShortDescription()
    {
//        String str = this.id+": Regular Vertex\n        statement: " + this.getInstruction() + "\n";
        String str = "<html>"+this.id+": Regular Vertex<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
                    + "statement: " + Parameters.getHTMLVerbatim(this.getInstruction()) + "</html>";
        return ""+str;
    }
    
	public MethodVertex getMergeParent()
	{
		return this.mergeParent;
	}
	
	public Method getMethod()
	{
		return this.method;
	}
	
	public String getMethodName()
	{
		return this.method.getFullName();
	}
	
	public ArrayList<AbstractVertex> getMergeChildren()
	{
		return new ArrayList<AbstractVertex>();
	}
	
    
	public void addNeighbor(Vertex dest)
	{
		if(Parameters.debug)
			System.out.println("adding edge from " + this.id + " to " + dest.id);

		this.neighbors.add(dest);
		dest.incoming.add(this);
		
		if(dest.id < this.id)
			return;
		
		boolean flag = false;

		//When we add an edge, we must check whether it creates a loop that requires us
		//to modify our layout.
		Method newMethod = ((Vertex) dest.parent).getMethod();
		if(dest.parent == Main.graph.root || newMethod.equals(dest.getMethod()))
		{
			Vertex ver = this;
			while(ver != Main.graph.root)
			{
				if(ver == dest)
				{
					flag = true;
					break;
				}
				ver = (Vertex) ver.parent;
			}
			
			if(flag) // if there is loop then, try to find potential point to break the loop 
			{
				boolean cont = true, same = true;
				AbstractVertex parVer = null, chVer = null, potential;
				ver = this;
				while(ver != dest && cont)
				{
					Method parentMethod = ((Vertex) ver.parent).getMethod();
					for(int i = 0; i < ver.incoming.size(); i++)
					{
						potential = ver.incoming.get(i);
						Method potentialMethod = ((Vertex) potential).getMethod();
						if(potential == ver.parent)
							continue;
						if(same)
						{
							if(dest.parent != Main.graph.root && parentMethod.equals(dest.getMethod()) && !potentialMethod.equals(dest.getMethod()))
								continue;
						}
						if(ver.checkPotentialParent())
						{
							cont = false;
							parVer = potential;
							chVer = ver;
							break;
						}
					}
					if(!parentMethod.equals(ver.getMethod()))
						same = false;
					ver = (Vertex) ver.parent;
				}

				if(cont)
					return;
				
				
				chVer.changeParent(parVer);
			}

			dest.changeParent(this);
		}
	}
	
	public String getInstruction()
	{
		return this.instruction;
	}
	
	public String getDescription()
	{
		return this.description;
	}
	
	public void setDescription(String desc)
	{
		this.description = desc;
	}
	
	public void setInstruction(String in)
	{
		this.instruction = in;
	}
	
	public void setNameToInstruction()
	{
		this.name = this.instruction;
	}
		
	public String getName()
	{
		return this.id + "\n" + this.getMethodName() + "\n" + this.getInstruction();
	}
	
	public void setLoopHeader(Vertex v)
	{
		this.loopHeader = v;
	}
	
	public Vertex getLoopHeader()
	{
		return this.loopHeader;
	}
	
	public void addLoopChild(Vertex v)
	{
		if(this != v)
			loopChildren.add(v);
		else
			System.out.println("Error! Cannot add self as header in loop decomposition.");
	}
	
	public ArrayList<Vertex> getLoopChildren()
	{
		return loopChildren;
	}
	
	//If add is true, we're adding to highlight this cycle. Otherwise, we're removing
	//a highlighted vertex.
	public void highlightCycles()
	{
		//First try to highlight the next largest loop containing this vertex
		if(this.loopHeader != null)
		{
			Vertex header = this.loopHeader;
			header.addHighlight(false, false, true, true);
			
			for(Vertex v : header.loopChildren)
				v.addHighlight(false, false, true, true);
		}
		
		//Otherwise try to highlight the loop that this vertex starts
		else if(this.loopChildren.size() > 0)
		{
			this.addHighlight(false, false, true, true);
			
			for(Vertex v : this.loopChildren)
				v.addHighlight(false, false, true, true);
		}
	}
	
	public void mergeByMethod(MethodVertex mergeVer) {
		MethodVertex ver = mergeVer;

		if (this.method == null)
		{
			System.out.println("ERROR! Uninitialized method for vertex ");
			System.exit(-1);
		}

		//Initialize first method vertex
		if (ver == null)
		{
			Main.graph.totalVertices++;
			ver = new MethodVertex(Main.graph.totalVertices, Main.graph.methodVertices.size(), this.method, false);
			ver.mergeRoot = this;
			Main.graph.methodVertices.add(ver);
		}

		//Create new method vertex
		if (!ver.getMethod().equals(this.method))
		{
			Main.graph.totalVertices++;
			ver = new MethodVertex(Main.graph.totalVertices, Main.graph.methodVertices.size(), this.method, false);
			ver.mergeRoot = this;
			Main.graph.methodVertices.add(ver);
		}

		this.mergeParent = ver;
		ver.mergeChildren.add(this);

		if (!this.drawEdges)
		{
			System.out.println("Removing edges for method: " + this.getMethodName());
			ver.drawEdges = false;
		}

		for (AbstractVertex v : this.children)
			((Vertex) v).mergeByMethod(ver);
	}

	public int getOutDegree()
	{
		return this.neighbors.size();
	}
}