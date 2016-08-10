
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;

import java.awt.geom.GeneralPath;
import java.awt.geom.Path2D;


public class VizPanel extends JPanel
{
	private boolean context;
	private StacFrame parent;
	private double maxWidth, maxHeight, minWidth, minHeight, boxWidth, boxHeight;

	//The leftMargin margin is the width of the panel minus the width of the currently displayed graph, divided by 2.
	//The topMargin margin is the height of the panel minus the height of the currently displayed graph, divided by 2.
	private double leftMargin, topMargin;

	public boolean showSelection = false;
	public double selectLeft, selectRight, selectTop, selectBottom;
	public double boxSize = 1.0;
	public static float hues[]; //Used for shading edges from green to red
	public boolean showEdge = true;

	public VizPanel(StacFrame p, boolean cont)
	{
		super();
		this.parent = p;
		this.context = cont;
	}
	
	public void vizPaint(Graphics2D g)
	{		
		Graph graph = Main.graph;

		//Provide a margin of 2% on each side?
		this.boxWidth = this.getWidth()*0.96/(graph.getWidth()*(graph.currWindow.right-graph.currWindow.left));
		this.boxHeight = this.getHeight()*0.96/(graph.getHeight()*(graph.currWindow.bottom-graph.currWindow.top));
		
		if(this.boxWidth > this.maxWidth)
		{
			this.boxWidth = this.maxWidth;
		}

		if(this.boxHeight > this.maxHeight)
		{
			this.boxHeight = this.maxHeight;
		}

		this.leftMargin = (this.getWidth() - this.boxWidth*graph.getWidth()*(graph.currWindow.right - graph.currWindow.left))/2;
		this.topMargin = (this.getHeight() - this.boxHeight*graph.getHeight()*(graph.currWindow.bottom - graph.currWindow.top))/2;

		drawEdges(g);
		drawVertices(g);
		
		//Draw arrows toward highlighted vertices that are off the main screen
		//drawHighlightedVertexMap(g);
	}
	
	public static void computeHues()
	{
		float start = 0.4f; //green
		float end = 0.0f; //red
		
		int maxLoopHeight = 0;
		for(Vertex v : Main.graph.vertices)
		{
			if(v.loopHeight > maxLoopHeight)
				maxLoopHeight = v.loopHeight;
		}
		
		System.out.println("Max loop height: " + maxLoopHeight);
		
		hues = new float[maxLoopHeight + 1];
		for(int i = 0; i <= maxLoopHeight; i++)
		{
			hues[i] = start - ((float) i)/(maxLoopHeight + 1)*(start - end);
		}
	}
	
	//Draw all visible graph vertices.
	public void drawVertices(Graphics2D g)
	{
		for(Vertex ver : Main.graph.vertices)
		{
			if(ver.isVisible)
				drawVertex(g, ver);
		}

		for(MethodVertex ver : Main.graph.methodVertices)
		{
			if(ver.isVisible)
				drawVertex(g, ver);
		}
		
		for(MethodPathVertex ver : Main.graph.methodPathVertices)
		{
			if(ver.isVisible)
				drawVertex(g, ver);
		}
	}
	
	public void drawVertex(Graphics2D g, AbstractVertex ver)
	{
		Graph graph = Main.graph;
		double x1temp, x2temp, y1temp, y2temp;
		int x1, x2, y1, y2;
		double fac = 0.25*this.boxSize;
		
		if(this.boxWidth < this.minWidth)
		{
			x1temp = ver.x - fac*this.minWidth/this.boxWidth;
			x2temp = ver.x + fac*this.minWidth/this.boxWidth;
		}
		else
		{
			x1temp = ver.x - fac;
			x2temp = ver.x + fac;
		}
		
		if(!this.context)
		{
			x1temp -= graph.currWindow.left*graph.getWidth();
			x2temp -= graph.currWindow.left*graph.getWidth();
		}
		
		x1 = (int) this.getPixelXFromIndex(x1temp);
		x2 = (int) this.getPixelXFromIndex(x2temp);
		
		
		if(this.boxHeight < this.minHeight)
		{
			y1temp = ver.y - fac*this.minHeight/this.boxHeight;
			y2temp = ver.y + fac*this.minHeight/this.boxHeight;
		}
		else
		{
			y1temp = ver.y - fac;
			y2temp = ver.y + fac;
		}
		
		if(!this.context)
		{
			y1temp -= graph.currWindow.top*graph.getHeight();
			y2temp -= graph.currWindow.top*graph.getHeight();
		}
		
		y1 = (int) this.getPixelYFromIndex(y1temp);
		y2 = (int) this.getPixelYFromIndex(y2temp);
		
		if(Parameters.vertexHighlight && (ver.isSelected() || ver.isChildSelected()))
		{
			//System.out.println("Drawing highlighted vertex: " + ver.id);
			g.setColor(Parameters.colorHighlight);
			g.fillRect(x1, y1, x2 - x1, y2 - y1);
		}
		else if(Parameters.vertexHighlight && (ver.isHighlighted() || ver.isChildHighlighted()))
		{
			//System.out.println("Drawing highlighted vertex: " + ver.id);
			g.setColor(Parameters.colorSelection);
			g.fillRect(x1, y1, x2 - x1, y2 - y1);
		}
		else
		{
			//Hue ranges from green to red
			//Allows for varying shades up to 5 nested loops
			float hue = hues[0];
			if(ver.loopHeight > 0)
				hue = hues[Math.min(hues.length - 1, ver.loopHeight)];
			
			float sat = 1f;
			float brightness = 1f;
			Color c = getHSBColorT(hue, sat, brightness);
			g.setColor(c);
			g.fillRect(x1, y1, x2 - x1, y2 - y1);
		}

/*		
        for(Integer tg : ver.tags)
        {
            int t = tg.intValue();
            
            if(Main.graph.highlightedTags.get(t).booleanValue())
            {
                double l=0;
                if((x2-x1)/6.0 > (y2-y1)/4.0)
                    l = (y2-y1)/4.0;
                else
                    l = (x2-x1)/6.0;
                
                drawStar(g,x1+(x2-x1)/6,(y1+y2)/2,l);
            }
        }
*/
        
        Font ff = new Font("Serif", Font.BOLD, Parameters.font.getSize());
		if(!this.context)
			this.drawCenteredString(g, ver.getName(), (int) (this.getPixelXFromIndex(ver.x - graph.currWindow.left*graph.getWidth())),
					(int) (this.getPixelYFromIndex(ver.y-graph.currWindow.top*graph.getHeight())), ff, Color.BLACK);
		
		if(this.boxWidth < this.minWidth || this.boxHeight < this.minHeight)
			g.setColor(this.getColorT(Color.BLACK.getRGB()));
		else
			g.setColor(Color.BLACK);

		//Draw outline of boxes only in main window or in context for selected or highlighted vertices
		if(this.context)
		{
            if(Parameters.vertexHighlight && (ver.isSelected() || ver.isChildSelected() || ver.isHighlighted() || ver.isChildHighlighted()))
            {
                g.setStroke(new BasicStroke(3));
                g.drawRect(x1, y1, x2 - x1, y2 - y1);
            }
		}
        else
        {
            g.setStroke(new BasicStroke(1));
            if(Parameters.vertexHighlight && (ver.isSelected() || ver.isChildSelected() || ver.isHighlighted() || ver.isChildHighlighted()))
                g.setStroke(new BasicStroke(3));
            g.drawRect(x1, y1, x2 - x1, y2 - y1);
        }
        
        
        if(Parameters.pingStart && ver.isSelected)
        {
            int width = x2-x1;
            int height = y2-y1;
            if(width>height)
                height = width;
            else
                width = height;
            
            int x = (x1+x2)/2 - width;
            int y = (y1+y2)/2 - height;
            
            g.setColor(Color.BLUE);
            g.drawOval(x,y,2*width,2*height);
            
            if(Parameters.pingEnd)
            {
                if(this.context)
                    Parameters.pingRespondedContext = true;
                else
                    Parameters.pingRespondedMain = true;
                if(Parameters.pingRespondedMain && Parameters.pingRespondedContext)
                    Parameters.pingStart = false;
            }
        }
        
        
	}
    
    //not needed anymore
    public void drawStar(Graphics2D g, double x, double y, double l)
    {
        double root3 = Math.sqrt(3);
        double l1=l;
        double l2=l/root3;
        Path2D.Double star = new Path2D.Double();
        star.moveTo(x+l2,y);
        star.lineTo(x+l1*root3/2,y-l/2);
        star.lineTo(x+l2/2,y-l2*root3/2);
        star.lineTo(x,y-l1);
        star.lineTo(x-l2/2,y-l2*root3/2);
        star.lineTo(x-l1*root3/2,y-l1/2);
        star.lineTo(x-l2,y);
        star.lineTo(x-l1*root3/2,y+l1/2);
        star.lineTo(x-l2/2,y+l2*root3/2);
        star.lineTo(x,y+l1);
        star.lineTo(x+l2/2,y+l2*root3/2);
        star.lineTo(x+l1*root3/2,y+l1/2);
        star.lineTo(x+l2,y);
        
        
        Color c = new Color(128,0,128);
        g.setColor(c);
        g.fill(star);
        g.setColor(Color.BLACK);
        g.draw(star);
        
    }
	
	//If edges are turned on, draw all visible graph edges.
	public void drawEdges(Graphics2D g)
	{
		if(this.showEdge)
		{
			for(Vertex ver : Main.graph.vertices)
			{
				AbstractVertex v = ver;
				while(!v.isVisible)
					v = v.getMergeParent();
				
				for(Vertex nbr : ver.neighbors)
				{
					AbstractVertex w = nbr;
					while(!w.isVisible)
						w = w.getMergeParent();

					//System.out.println("Drawing edge: " + v.getFullName() + ", " + w.getFullName());
					if(v != w && v.drawEdges && w.drawEdges)
						drawEdge(g, v, w);
				}
			}
		}
	}
	
	//Checks if an edge between two different vertices should be highlighted
	//We highlight it only if the vertices on both ends want it to be highlighted.
	public boolean isHighlightedEdge(AbstractVertex v, AbstractVertex nbr)
	{
		if(!v.isOutgoingHighlighted || !nbr.isIncomingHighlighted)
			return false;
		else if(Parameters.highlightOutgoing && !Parameters.highlightIncoming && v.isOutgoingHighlighted)
			return false;
		else return !(Parameters.highlightIncoming && !Parameters.highlightOutgoing && nbr.isIncomingHighlighted);
	}
	
	public void drawEdge(Graphics2D g, AbstractVertex v, AbstractVertex w)
	{
		Graph graph = Main.graph;
		double x1temp, x2temp, y1temp, y2temp;
		int x1, x2, y1, y2;
		double fac = 0.25*this.boxSize;
		boolean isCurved;
		
		
		x1temp = v.x;
		x2temp = w.x;
		
		if(!this.context)
		{
			x1temp -= graph.getWidth()*graph.currWindow.left;
			x2temp -= graph.getWidth()*graph.currWindow.left;
		}
		
		x1 = (int) this.getPixelXFromIndex(x1temp);
		x2 = (int) this.getPixelXFromIndex(x2temp);
		
		//Start of arrow is at center of first box
		y1temp = v.y;
		
		//If box 1 is above box 2, draw arrow to the center of the topMargin line
		if(v.y > w.y)
		{
			isCurved = true;
			if(this.boxHeight < this.minHeight)
			{
				y2temp = w.y + fac*this.minHeight/this.boxHeight;
			}
			else
			{
				y2temp = w.y + fac;
			}
		}
		
		//Otherwise, draw arrow to the center of the bottom line
		else
		{
			isCurved = false;
			if(this.boxHeight < this.minHeight)
			{
				y2temp = w.y - fac*this.minHeight/this.boxHeight;
			}
			else
			{
				y2temp = w.y - fac;
			}
		}
		
		if(!this.context)
		{
			y1temp -= graph.currWindow.top*graph.getHeight();
			y2temp -= graph.currWindow.top*graph.getHeight();
		}
		
		y1 = (int) this.getPixelYFromIndex(y1temp);
		y2 = (int) this.getPixelYFromIndex(y2temp);

		
		if(isHighlightedEdge(v, w))
		{
			g.setColor(Color.RED);
			g.setStroke(new BasicStroke(4));
		}
		else
		{
			g.setColor(Color.BLACK);
			g.setStroke(new BasicStroke(1));
		}
		
		if(!isCurved)
			drawArrow(g, x1, y1, x2, y2);
		else
			drawCurvedArrow(g, x1, y1, x2, y2);
	}
	
	public void drawArrow(Graphics2D g, int x1, int y1, int x2, int y2)
	{
		g.drawLine(x1, y1, x2, y2);
		double angle = Math.atan2(y2 - y1, x2 - x1);		
		double length = 15.0;
		if(this.boxSize < 1)
			length = length * this.boxSize;

		if(length < 2.0 && !this.context)
			length = 2.0;
		if(length < 1.0 && this.context)
			length = 1.0;
		
		this.drawArrowhead(g, x2, y2, angle, length);
	}
	
	public void drawCurvedArrow(Graphics2D g, int x1, int y1, int x2, int y2)
	{
		//Our arrow will start and end pointing toward our control point.
		//We pick the control point by starting at the midpoint of the line from (x1, y1) to (x2, y2),
		//and moving a distance of Parameters.minBoxWidth from this line perpendicularly.
		
		int x3, y3;
		int midX = (x1 + x2)/2;
		int midY = (y1 + y2)/2;

		//This sets the incoming/outgoing angle to be arctan(1/2), or about 26 degrees. It looks okay.
		double dist = 0.5*getEuclideanDistance(x1, y1, midX, midY);
		
		if(x1 == x2)
		{
			x3 = (int) (midX - dist);
			y3 = midY;
		}
		else
		{
			//We know that y2 > y1, since our arrow is pointing up.
			double mPerp = ((double) x2 - x1)/(y2 - y1);
			double deltaX = dist/Math.sqrt(mPerp*mPerp + 1);
			double deltaY = (dist*mPerp)/Math.sqrt(mPerp*mPerp + 1);
			
			if(x2 > x1)
			{
				x3 = (int) (midX + deltaX);
				y3 = (int) (midY - deltaY);
			}
			else
			{
				x3 = (int) (midX - deltaX);
				y3 = (int) (midY + deltaY);
			}
		}
		
		GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD, 3);
		path.moveTo(x1, y1);
		path.quadTo(x3, y3, x2, y2);
		g.draw(path);
		
		double angle = Math.atan2(y2 - y3, x2 - x3);
		double length = 15.0;
		if(this.boxSize < 1)
			length = length * this.boxSize;

		if(length < 2.0)
			length = 2.0;
		
		this.drawArrowhead(g, x2, y2, angle, length);
	}

	private double getEuclideanDistance(double x1, double y1, double x2, double y2)
	{
		return Math.sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2));
	}
	
	public void drawArrowhead(Graphics2D g, int x, int y, double angle, double length)
	{
		double angleDiff = 20.0*Math.PI/180.0; //30 degrees
		
		//For the legs of our arrow, rotate the opposite direction, then offset by angleDiff degrees.
		double anglePlus = (angle + Math.PI) + angleDiff;
		double angleMinus = (angle + Math.PI) - angleDiff;
		int xPlus = (int)(x + length*Math.cos(anglePlus));
		int yPlus = (int)(y + length*Math.sin(anglePlus));
		int xMinus = (int)(x + length*Math.cos(angleMinus));
		int yMinus = (int)(y + length*Math.sin(angleMinus));
		
		/*System.out.println("Head point: " + x + ", " + y);
		System.out.println("Angle: " + angle*180.0/Math.PI + " degrees");
		System.out.println("Plus point: " + xPlus + ", " + yPlus);
		System.out.println("Minus point: " + xMinus + ", " + yMinus);*/
		
		GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD, 4);
		path.moveTo(x, y);
		path.lineTo(xPlus, yPlus);
		path.lineTo(xMinus, yMinus);
		path.lineTo(x, y);
		g.fill(path);
	}
	
	public void drawCenteredString(Graphics2D g, String text, int x, int y, Font font, Color col)
	{
	    g.setFont(font);
	    g.setColor(col);
	    FontMetrics metrics = g.getFontMetrics(font);
	    
	    double width;
	    if(this.boxWidth < this.minWidth)
	    {
	    	width = this.minWidth * 0.5 * this.boxSize;
	    }
	    else
	    {
	    	width = this.boxWidth * 0.5 * this.boxSize;
	    }
	    
	    double height;
	    if(this.boxHeight < this.minHeight)
	    {
	    	height = this.minHeight * 0.5 * this.boxSize;
	    }
	    else
	    {
	    	height = this.boxHeight * 0.5 * this.boxSize;
	    }

	    String[] lines = text.split("\n");
	    
	    height = height/metrics.getHeight();
	    if(height<1)
	    	height = 1;
	    int i;
	    for(i=0; i<lines.length-height; i++);
	    for(; i < lines.length; i++)
	    {
		    String line = lines[i];
		    if(width < metrics.stringWidth(line))
		    {
		    	line = shortenString(line, width, metrics);
		    }
		    
		    int xCentered = x - metrics.stringWidth(line)/2;
		    g.drawString(line, xCentered, y);
		    y += metrics.getHeight();
	    }
	}
	
	//Do a binary search to see how much of the line we can print.
	public String shortenString(String str, double width, FontMetrics metrics)
	{
    	int start = 0;
    	int end = str.length();
    	while(end - start > 1)
    	{
    		int center = (start + end)/2;
    		String newSubstr = str.substring(0, center) + "..";
    		if(width < metrics.stringWidth(newSubstr))
    			end = center;
    		else
    			start = center;
    	}

    	String newSubstr1 = str.substring(0, start) + "..";
    	String newSubstr2 = str.substring(0, end) + "..";
    	if(width < metrics.stringWidth(newSubstr2))
    		str = newSubstr1;
    	else
    		str = newSubstr2;
    	
    	return str;
	}
	
	/*public void drawHighlightedVertexMap(Graphics2D g)
	{
		double leftMargin = 0;
		double right = (Main.graph.currWindow.right - Main.graph.currWindow.leftMargin)*Main.graph.getWidth();
		double topMargin = 0;
		double bottom = (Main.graph.currWindow.bottom - Main.graph.currWindow.topMargin)*Main.graph.getHeight();
		double middleX = (leftMargin + right)/2.0;
		double middleY = (topMargin + bottom)/2.0;
		
		int[] countVertices = new int[9];
		for(int i = 0; i < 9; i++)
		{
			countVertices[i] = 0;
		}
		
		for(AbstractVertex v : highlightedVertices)
		{
			//We split our window by the leftMargin, right, topMargin, and bottom of the current view.
			//This gives us nine possible regions. The eight outer regions are not contained
			//in our current view, so we add an arrow for each one with the number of highlighted
			//vertices it contains.
			/* _____________
			 * | 1 | 2 | 3 |
			 * _____________
			 * | 4 | 5 | 6 |
			 * _____________
			 * | 7 | 8 | 9 |
			 * _____________
			 *
			
			int currIndex = 0;
			//x-position: leftMargin side = 0, middle = 1, right = 2
			if(v.x >= leftMargin && v.x <= right)
			{
				currIndex++;
			}
			else if(v.x > right)
			{
				currIndex += 2;
			}
			
			//y-position: topMargin = 0, middle = 3, bottom = 6
			if(v.y >= topMargin && v.y <= bottom)
			{
				currIndex += 3;
			}
			else if(v.y > bottom)
			{
				currIndex += 6;
			}
			
			countVertices[currIndex]++;
		}
		
		double[] arrowAngle = {225, 270, 315, 180, -1, 0, 135, 90, 45};
		double[] xPositions = {leftMargin, middleX, right, leftMargin, -1, right, leftMargin, middleX, right};
		double[] yPositions = {topMargin, topMargin, topMargin, middleY, middleY, middleY, bottom, bottom, bottom};
		int arrowLength = 20;
		
		g.setColor(Color.BLACK);
		g.setStroke(new BasicStroke(2));
		
		for(int i = 0; i < 9; i++)
		{
			//Take arrow pointing to right, place tip at xPositions[i], yPositions[i], and rotate by arrowAngle[i]
			//Then label with value
			if(i != 4)
			{
				arrowAngle[i] *= Math.PI/180.0; //Convert angles to radians
				int x1 = (int) (this.getRelativeXPixels(xPositions[i]));
				int y1 = (int) (this.getRelativeYPixels(yPositions[i]));
				int x2 = x1 + (int)(arrowLength*Math.cos(arrowAngle[i]));
				int y2 = y1 + (int)(arrowLength*Math.sin(arrowAngle[i]));
				//System.out.println(xPositions[i] + ", " + yPositions[i]);
				//System.out.println(x1 + ", " + y1);
				
				drawArrow(g, x1, y1, x2, y2);
			}
		}
	}*/
	
	public void contextPaint(Graphics2D g)
	{
		Graph graph = Main.graph;

		this.boxWidth = this.getWidth()*0.96/graph.getWidth();
		this.boxHeight = this.getHeight()*0.96/graph.getHeight();

		if(this.boxWidth >this.maxWidth)
			this.boxWidth = this.maxWidth;

		if(this.boxHeight >this.maxHeight)
			this.boxHeight = this.maxHeight;
		
		this.leftMargin = (this.getWidth() - this.boxWidth *graph.getWidth())/2;
		this.topMargin = (this.getHeight() - this.boxHeight *graph.getHeight())/2;

		drawEdges(g);
		drawVertices(g);
	}
	
	public Color getHSBColorT(float H, float S, float B)
	{
		int rgb = Color.HSBtoRGB(H, S, B);
	    int red = (rgb >> 16) & 0xFF;
	    int green = (rgb >> 8) & 0xFF;
	    int blue = rgb & 0xFF;
	    Color color = new Color(red, green, blue, Parameters.transparency);
	    return color;
	}
	
	public Color getColorT(int rgb)
	{
	    int red = (rgb >> 16) & 0xFF;
	    int green = (rgb >> 8) & 0xFF;
	    int blue = rgb & 0xFF;
	    Color color = new Color(red, green, blue, Parameters.transparency);
	    return color;
	}
	
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
		this.setBackground(Color.WHITE);

		if(Main.graph == null)
			return;
		
		this.minWidth = Parameters.minBoxWidth;
		this.minHeight = Parameters.minBoxHeight;
		this.maxWidth = this.getWidth();
		this.maxHeight = this.getHeight();
		
		if(this.maxWidth<this.minWidth)
			this.maxWidth = this.minWidth;
		if(this.maxHeight<this.minHeight)
			this.maxHeight = this.minHeight;
		
		Graphics2D g2 = (Graphics2D) g.create();
		
		int x1, x2, y1, y2;
		
		Graph graph = Main.graph;
		
		
		if(this.context)
		{
			g.setColor(Parameters.colorFocus);
			
			if(graph.currWindow.left > 0 || graph.currWindow.right < 1
					|| graph.currWindow.top > 0 || graph.currWindow.bottom < 1)
			{
				x1 = (int) this.getPixelXFromIndex(graph.getWidth()*graph.currWindow.left);
				y1 = (int) this.getPixelYFromIndex(graph.getHeight()*graph.currWindow.top);
				x2 = (int) this.getPixelXFromIndex(graph.getWidth()*graph.currWindow.right);
				y2 = (int) this.getPixelYFromIndex(graph.getHeight()*graph.currWindow.bottom);
				g.fillRect(x1, y1, x2-x1, y2-y1);
				g.setColor(Color.YELLOW);
				g.drawRect(x1, y1, x2-x1, y2-y1);
			}
		}

		if(this.showSelection)
		{
			g2.setColor(Parameters.colorSelection);

			if(this.selectLeft < this.selectRight)
			{
				x1 = (int) this.selectLeft;
				x2 = (int) this.selectRight;
			}
			else
			{
				x2 = (int) this.selectLeft;
				x1 = (int) this.selectRight;
			}

			if(this.selectTop < this.selectBottom)
			{
				y1 = (int) this.selectTop;
				y2 = (int) this.selectBottom;
			}
			else
			{
				y2 = (int) this.selectTop;
				y1 = (int) this.selectBottom;
			}
						
			g2.fillRect(x1, y1, x2-x1, y2-y1);
			
			float dash[] = {10.0f};
		    BasicStroke dashed = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash, 0.0f);
		    g2.setStroke(dashed);
		    g2.setColor(Color.GRAY);
		    g2.drawRect(x1, y1, x2-x1, y2-y1);
		}

		
		
		if(this.context)
			this.contextPaint(g2);
		else
        {
			this.vizPaint(g2);
        }
		
		g.dispose();
	}

	//Convert a current pixel location to a horizontal value between 0 and 1
	public double getRelativeFracFromAbsolutePixelX(double x)
	{
		Graph graph = Main.graph;
		if(this.context)
		{
			double xFrac = (x - this.leftMargin) / (this.boxWidth*graph.getWidth());
			if(xFrac < 0)
				return 0;
			else if(xFrac > 1)
				return 1;
			else
				return xFrac;
		}
		else
		{
			double xFrac = (x - this.leftMargin) / (this.boxWidth*graph.getWidth()) + graph.currWindow.left;
			if(xFrac < 0)
				return 0;
			else if(xFrac > 1)
				return 1;
			else
				return xFrac;
		}
	}

	//Convert an absolute pixel location to a vertical value between 0 and 1
	public double getRelativeFracFromAbsolutePixelY(double y)
	{
		Graph graph = Main.graph;
		if(this.context)
		{
			double yFrac = (y - this.topMargin) / (this.boxHeight * graph.getHeight());
			if(yFrac < 0)
				return 0;
			else if(yFrac > 1)
				return 1;
			else
				return yFrac;
		}
		else
		{
			double yFrac = (y - this.topMargin) / (this.boxHeight * graph.getHeight()) + graph.currWindow.top;
			if(yFrac < 0)
				return 0;
			else if(yFrac > 1)
				return 1;
			else
				return yFrac;
		}
	}

	//Convert a horizontal box index to a current x pixel location
	public double getPixelXFromIndex(double x)
	{
		return this.leftMargin + this.boxWidth * x;
	}

	//Convert a vertical box index to a current y pixel location
	public double getPixelYFromIndex(double y)
	{
		return this.topMargin + this.boxHeight * y;
	}

	public double getIndexFromCurrentPixelX(double currXPixel)
	{
		double xFrac = getRelativeFracFromAbsolutePixelX(currXPixel);
		return xFrac*Main.graph.getWidth();
	}

	public double getIndexFromCurrentPixelY(double currYPixel)
	{
		double yFrac = getRelativeFracFromAbsolutePixelY(currYPixel);
		return yFrac*Main.graph.getHeight();
	}

	public StacFrame getParent()
	{
		return parent;
	}
}
