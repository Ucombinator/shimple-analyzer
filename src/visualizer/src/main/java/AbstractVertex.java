
import javax.swing.tree.DefaultMutableTreeNode;
import java.util.ArrayList;

//The base class for the various kinds of vertices.
//An abstract vertex is a box on the screen. The subclasses vary in
//what kind of data the vertex represents.

//The current subclasses are lines of code, methods, and paths of methods.
//We may also add other kinds of collapsing later, which will require
//having new kinds of vertices.

public abstract class AbstractVertex
{
    public ArrayList<Integer> tags;

    public enum VertexType
	{
		LINE, METHOD, METHOD_PATH
	}

	protected int id;
	protected VertexType vertexType;
	protected String name;
	protected int index, parentIndex;

	//children stores all of the vertices to which we have edges from this vertex
	//in the current display
	//incoming stores all of the vertices that have edges into this vertex in
	//the current display
	//The base graph is stored in the neighbors in the Vertex class.
	protected ArrayList<AbstractVertex> children, incoming;

	//These are the coordinates of a subtree (maybe?)
	protected double left, right, top, bottom;
	protected double x, y, width, height;
	
	//The merge start is the first vertex of the merge children for a merge vertex, and is used
	//to create its incoming edges.
	protected AbstractVertex parent, mergeRoot;
	protected boolean isVisible, mergeable;
	protected boolean isSelected, isHighlighted, isIncomingHighlighted, isOutgoingHighlighted; //Select or Highlight this vertex, incoming edges, or outgoing edges
	protected boolean drawEdges;
	protected int numChildrenHighlighted, numChildrenSelected;
	protected int loopHeight;
	
	//Subclasses must override these so that we have descriptions for each of them,
	//and so that our generic collapsing can work for all of them
	abstract String getRightPanelContent();
    abstract String getShortDescription();
	abstract AbstractVertex getMergeParent();
	abstract ArrayList<? extends AbstractVertex> getMergeChildren();
	abstract String getName();
	
	public void setDefaults()
	{
		this.incoming = new ArrayList<AbstractVertex>();
		this.children = new ArrayList<AbstractVertex>();
        this.tags = new ArrayList<Integer>();
		
		this.width = 1;
		this.height = 1;
		this.left = 0;
		this.right = 1;
		this.top = -1;
		this.bottom = 0;
		this.x = 0.5;
		this.y = -0.5;
	}
	
    
    public DefaultMutableTreeNode toDefaultMutableTreeNode()
    {
//        return new DefaultMutableTreeNode(this.getShortDescription());
        return new DefaultMutableTreeNode(this);
    }
    
    public void addTag(int t)
    {
        for(Integer i : this.tags)
        {
            if(i.intValue()==t)
                return;
        }
        this.tags.add(new Integer(t));
    }
    
    public boolean hasTag(int t)
    {
        for(Integer i : this.tags)
        {
            if(i.intValue()==t)
                return true;
        }
        return false;
    }
    
	public double distTo(double x, double y)
	{
		return Math.sqrt((x - this.x)*(x - this.x) + (y - this.y)*(y - this.y));
	}
	
	public void replaceChild(AbstractVertex ver)
	{
		int p = ver.parentIndex;
		double inc = ver.width - this.children.get(p).width;
		
		this.children.remove(p);
		this.children.add(p, ver);
		this.increaseWidth(ver, inc);
		
		this.calculateHeight();
	}
	
	public void calculateHeight()
	{
		double h = 0;
		for(int i = 0; i < this.children.size(); i++)
		{
			if(this.children.get(i).height > h)
				h = this.children.get(i).height;
		}
		this.height = h + 1;
		this.bottom = this.top + this.height;
		
		if(this.parent != null)
			this.parent.calculateHeight();
	}

	//Collapse a merge parent's vertices.
	public void collapse()
	{
		//To collapse a vertex, first we check that it contains visible merge children.
		if(this.mergeRoot != null && this.mergeRoot.isVisible)
		{
			this.children = new ArrayList<AbstractVertex>();

			//Set the location for our merge parent to be the same as its first child.
			this.left = this.mergeRoot.left;
			this.top = this.mergeRoot.top;
			Main.graph.setMaxHeight(0);

			//Remove the children of our merge parent and set them invisible.
			double w = this.mergeRoot.disappear(this.left, this.top + 1, this);
			if(w > 1)
				this.width = w;
			else
				this.width = 1;
			
			this.right = this.left + this.width;
			this.x = (this.left + this.right)/2;
			
			this.height = Main.graph.getMaxHeight() + 1;
			this.bottom = this.top + this.height;
			this.y = this.top + 0.5;
	
			this.parent = this.mergeRoot.parent;
			this.parentIndex = this.mergeRoot.parentIndex;
			this.mergeRoot.parent.replaceChild(this);
	
			this.isVisible = true;
		}
	}

	//Collapse a vertex into its merge parent
	private double disappear(double left, double top, AbstractVertex mP)
	{
		double w = 0;
		AbstractVertex v;
		
		for(int i = 0; i < this.children.size(); i++)
		{
			v = this.children.get(i);
			if(v.getMergeParent() == mP)
				w = w + v.disappear(left + w, top, mP);
			else
			{
				while(!v.isVisible && v.getMergeParent() != null)
					v = v.getMergeParent();

				while(!v.isVisible)
					v = v.mergeRoot;

				v.shiftSubtree(left + w - v.left);
				v.shiftSubtreeY(top - v.top);

				if(v.height > Main.graph.getMaxHeight())
					Main.graph.setMaxHeight(v.height);

				v.parent = mP;
				v.parentIndex = mP.children.size();
				mP.children.add(v);
				w += v.width;
			}
		}
		this.isVisible = false;
		return w;
	}

	//Expand a vertex out of its merge parent.
	public void deCollapse()
	{
		//First check that our vertex is expandable.
		//It must be visible and have a valid merge root.
		if(this.isVisible || this.mergeRoot != null)
		{
			//If so, we set the merge start vertex to take its parent's location...
			this.mergeRoot.left = this.left;
			this.mergeRoot.top = this.top;

			//Show it and its children in our graph...
			this.mergeRoot.appear(this.left, this.top, this);

			//Connect its edges...
			this.mergeRoot.parent = this.parent;
			this.mergeRoot.parentIndex = this.parentIndex;
			this.parent.replaceChild(this.mergeRoot);

			//And lastly, we set our current vertex to be invisible.
			this.isVisible = false;
		}
	}

	//Beginning with our starting merge vertex, display all children of the
	//expanding merge parent.
	private void appear(double left, double top, AbstractVertex mP)
	{
		double w = 0;
		AbstractVertex v;
		
		this.left = left;
		this.top = top;
		this.y = this.top + 0.5;
		
		for(int i = 0; i < this.children.size(); i++)
		{
			//Check each of our children. All merge siblings should appear.
			v = this.children.get(i);
			if(v.getMergeParent() == mP)
			{
				v.appear(left + w, top + 1, mP);
				w += v.width;
			}
			//Vertices that do not have the same merge parent do not need to be changed,
			//but we must recompute our edges to them.
			else
			{
				//We walk up our merge tree until we cannot go higher, or we reach a
				//vertex that is expanded.
				while(!v.isVisible && v.getMergeParent() != null)
					v = v.getMergeParent();

				//Then we walk back down the chain of merge roots until we find one that
				//is visible. This should be the child to which we have an edge.
				while(!v.isVisible)
					v = v.mergeRoot;

				//If our current child is not correct, we replace it. This can happen when
				//the current child is either collapsed or expanded to a different level
				//than it was before.
				if(v != this.children.get(i))
				{
					this.children.remove(i);
					this.children.add(i, v);
				}

				//We must shift our subtrees, since our children have changed.
				v.shiftSubtree(left + w - v.left);
				v.shiftSubtreeY(top + 1 - v.top);
				v.parent = this;
				v.parentIndex = i;
				w += v.width;
			}
		}
		
		if(w > 1)
			this.width = w;
		else
			this.width = 1;

		this.right = this.left + this.width;
		this.x = (this.left + this.right)/2;
		
		double h = 0;
		for(int i = 0; i < this.children.size(); i++)
		{
			if(this.children.get(i).height > h)
				h = this.children.get(i).height;
		}
		this.height = h + 1;
		this.bottom = this.top + this.height;
		
		this.isVisible = true;
	}
	
	public void increaseWidth(AbstractVertex child, double inc)
	{
		AbstractVertex ver = this;
		AbstractVertex ch = child;
		while(ver != null)
		{
			ver.width += inc;
			ver.right += inc;
			ver.x = (ver.left + ver.right)/2;
			
			for(int i = ch.parentIndex + 1; i < ver.children.size(); i++)
			{
				ver.children.get(i).shiftSubtree(inc);
			}
			ch = ver;
			ver = ver.parent;
		}
	}

	//Shift a tree by the given increment by DFS
	public void shiftSubtree(double inc)
	{
		AbstractVertex ver = this;
		while(true)
		{
			ver.left += inc;
			ver.right += inc;
			ver.x += inc;

			if(ver.children.size() > 0)
				ver = ver.children.get(0);
			else
			{
				while(ver != this && ver.parent.children.size() == ver.parentIndex + 1)
				{
					ver = ver.parent;
				}
				if(ver == this)
					break;
				else
					ver = ver.parent.children.get(ver.parentIndex + 1);
			}
		}
	}

	//TODO: Why not use DFS here?
	public void shiftSubtreeY(double inc)
	{
		this.top += inc;
		this.bottom += inc;
		this.y += inc;
		
		for(int i = 0; i < this.children.size(); i++)
			this.children.get(i).shiftSubtreeY(inc);
	}
	
	public void increaseHeight(double inc)
	{
		this.height += inc;
		
		if(this.parent == null)
			return;
		if(this.height + 1 > this.parent.height)
			this.parent.increaseHeight(this.height - this.parent.height + 1);
	}
	
	public void addChild(AbstractVertex child)
	{
		child.parentIndex = this.children.size();
		this.children.add(child);
		child.parent = this;


		if(this.children.size() == 1)
		{
			child.shiftSubtree(this.left - child.left);
			if(child.width > 1)
				this.increaseWidth(child, child.width - 1);
		}
		else
		{
			child.shiftSubtree(this.right - child.left);
			this.increaseWidth(child, child.width);
		}

		child.shiftSubtreeY(this.bottom - child.top);
		
		if(child.height + 1 > this.height)
			this.increaseHeight(child.height - this.height + 1);
	}
	
	public boolean checkPotentialParent()
	{
		return this != Main.graph.root;
	}
	
	public void setLoopHeight()
	{
		if(this.mergeRoot ==null)
			return;
		
		this.loopHeight = 0;
		for(AbstractVertex v : this.getMergeChildren())
		{
			if(v.loopHeight > this.loopHeight)
				this.loopHeight = v.loopHeight;
		}
	}
	
	public void addHighlight(boolean select, boolean vertex, boolean to, boolean from)
	{
		//System.out.println("Highlighting vertex: " + this.getFullName());
		if(select)
			this.isSelected = true;
		if(vertex)
			this.isHighlighted = true;
		if(to)
			this.isIncomingHighlighted = true;
		if(from)
			this.isOutgoingHighlighted = true;

		AbstractVertex v = this.getMergeParent();
		while(v != null)
		{
			if(select)
			{
				v.numChildrenSelected++;
			}
			if(vertex)
			{
				v.numChildrenHighlighted++;
				//System.out.println("Adding child highlight to " + v.getFullName());
			}
			if(to)
				v.isIncomingHighlighted = true;
			if(from)
				v.isOutgoingHighlighted = true;
			
			v = v.getMergeParent();
		}
	}
	
	public boolean isCycleHighlighted()
	{
		return this.isOutgoingHighlighted && this.isIncomingHighlighted;
	}
	
	public boolean isHighlighted()
	{
		return this.isHighlighted;
	}
	
	
	public boolean isChildHighlighted()
	{
		//System.out.println("Highlighted children for vertex: " + this.getFullName() + " = " + this.numChildrenHighlighted);
		return this.numChildrenHighlighted > 0;
	}
	
    public boolean isParentHighlighted()
    {
        AbstractVertex ver = this;
        
        while(ver !=null)
        {
            if(ver.isHighlighted)
                return true;
            ver = ver.getMergeParent();
        }
        return false;
    }
    
    public boolean isBranchHighlighted()
    {
        return this.isHighlighted() | this.isParentHighlighted() | this.isChildHighlighted();
    }
    
	public boolean isSelected()
	{
		return this.isSelected;
	}
	
	public boolean isChildSelected()
	{
		return this.numChildrenSelected > 0;
	}
    
    public boolean isParentSelected()
    {
        AbstractVertex ver = this;
        
        while(ver !=null)
        {
            if(ver.isSelected)
                return true;
            ver = ver.getMergeParent();
        }
        return false;
    }
    
    public boolean isBranchSelected()
    {
        return this.isSelected() | this.isParentSelected() | this.isChildSelected();
    }
	
	public void clearAllHighlights()
	{
		if(this.isHighlighted)
		{
			AbstractVertex v = this.getMergeParent();
			while(v != null)
			{
				v.numChildrenHighlighted--;
				v = v.getMergeParent();
			}
		}

		this.isHighlighted = false;
		this.isIncomingHighlighted = false;
		this.isOutgoingHighlighted = false;
	}
	
	public void clearAllSelect()
	{
		if(this.isSelected)
		{
			AbstractVertex v = this.getMergeParent();
			while(v != null)
			{
				v.numChildrenSelected--;
				v = v.getMergeParent();
			}
		}

		this.isSelected = false;
		this.isIncomingHighlighted = false;
		this.isOutgoingHighlighted = false;
	}
	
	public void clearCycleHighlights()
	{
		this.isIncomingHighlighted = false;
		this.isOutgoingHighlighted = false;
	}
	
	public void changeParent(AbstractVertex newParent)
	{
		this.parent.increaseWidth(this, -1*this.width);
		for(int i = this.parentIndex + 1; i < this.parent.children.size(); i++)
		{
			this.parent.children.get(i).parentIndex--;
		}
		this.parent.children.remove(this.parentIndex);
		AbstractVertex ver = this;
		
		while(ver.parent != null)
		{
			double height = 0;
			for(int i=0; i< ver.parent.children.size(); i++)
			{
				if(ver.parent.children.get(i).height>height)
					height = ver.parent.children.get(i).height;
			}
			ver.parent.height = height + 1;
			ver = ver.parent;
		}
		
		newParent.addChild(this);
	}
	
	/*public void printCoordinates()
	{
		System.out.println(this.getFullName() + ": " + this.x + ", " + this.y + ", left = " + this.left + ", right = " + this.right +
				", width = " + this.width + ", top = " + this.top + ", bottom = " + this.bottom + ", height = " + this.height);
	}*/
}
