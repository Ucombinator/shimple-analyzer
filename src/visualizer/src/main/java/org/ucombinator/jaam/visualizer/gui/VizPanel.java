package org.ucombinator.jaam.visualizer.gui;

import javafx.animation.ScaleTransition;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Bounds;
import javafx.geometry.Point2D;

import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.ScrollEvent;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;
import javafx.scene.layout.Pane;
import javafx.scene.control.ScrollPane;
import javafx.scene.Group;

import java.awt.Color;

import java.util.HashSet;
import java.util.Iterator;

import org.ucombinator.jaam.visualizer.layout.AbstractLayoutVertex;
import org.ucombinator.jaam.visualizer.layout.LayoutEdge;
import org.ucombinator.jaam.visualizer.graph.Edge;
import org.ucombinator.jaam.visualizer.graph.Graph;
import org.ucombinator.jaam.visualizer.layout.LayoutAlgorithm;
import org.ucombinator.jaam.visualizer.layout.LayerFactory;
import org.ucombinator.jaam.visualizer.main.Main;
import org.ucombinator.jaam.visualizer.main.Parameters;

public class VizPanel extends JFXPanel
{
	private Group contentGroup;
	private Pane testPane;
	private ScrollPane scrollPane;
	public HashSet<AbstractLayoutVertex> highlighted;

	public static float hues[]; //Used for shading nodes from green to red
	private AbstractLayoutVertex panelRoot;
	private javafx.scene.paint.Color[] colors = {javafx.scene.paint.Color.AQUAMARINE,
			javafx.scene.paint.Color.GREEN, javafx.scene.paint.Color.AZURE,
			javafx.scene.paint.Color.BLUEVIOLET, javafx.scene.paint.Color.DARKTURQUOISE};
	public static int maxLoopHeight;

	// The dimensions of the background for our graph
	public double rootWidth = 500.0, rootHeight = 500.0;

	// Store the count for vertex width and height when everything is expanded
	public double maxVertexWidth, maxVertexHeight;

	public AbstractLayoutVertex getPanelRoot()
	{
		return this.panelRoot;
	}

	public VizPanel()
	{
		super();
		initContentGroup();
		highlighted = new HashSet<AbstractLayoutVertex>();
	}

	private void initContentGroup() {
		contentGroup = new Group();

		if (Parameters.debugMode)
		{
			testPane = new Pane();
			testPane.getChildren().add(contentGroup);
			this.setScene(new Scene(testPane));
		}
		else
		{
			scrollPane = createZoomPane(contentGroup);
			this.setScene(new Scene(scrollPane));
		}
		this.setBackground(Color.WHITE);		
	}

	public void initFX(AbstractLayoutVertex root)
	{
		if(root == null)
		{
			//System.out.println("Running layout...");
			Graph g = Main.graph;
			this.panelRoot = LayerFactory.getLayeredGraph(g);
			LayoutAlgorithm.layout(this.panelRoot);
			resetPanelSize();
		}
		else
		{
			this.panelRoot = root;
		}
		drawNodes(null, this.panelRoot);
		drawEdges(null, this.panelRoot);
	}
	

	public void resetPanelSize() {
		this.maxVertexWidth = this.panelRoot.getWidth();
		this.maxVertexHeight = this.panelRoot.getHeight();		
	}

	double factorX = 1;
	double factorY = 1;
	double factorMultiple = 1.1;
	double maxFactorMultiple = 3;

	public double scaleX(double coordinate)
	{
		return factorX*(coordinate * rootWidth / this.maxVertexWidth);
	}

	public double scaleY(double coordinate)
	{
		return factorY*(coordinate * rootHeight / this.maxVertexHeight);
	}

	public double invScaleX(double pixelCoordinate)
	{
		return (pixelCoordinate * this.maxVertexWidth / rootWidth);
	}

	public double invScaleY(double pixelCoordinate)
	{
		return (pixelCoordinate * this.maxVertexHeight / rootHeight);
	}

	public double getZoomLevel()
	{
		// We assume that scaleX and scaleY are equal
		return contentGroup.getScaleX();
	}

	// Divides the actual width in pixels by the width in vertex units
	public double getWidthPerVertex()
	{
		return panelRoot.getGraphics().getWidth() / panelRoot.getWidth();
	}

	//Called when the user clicks on a line in the left area.
	//Updates the vertex highlights to those that correspond to the instruction clicked.
	public void searchByJimpleIndex(String method, int index, boolean removeCurrent, boolean addChosen)
	{
		if(removeCurrent) {
			// Unhighlight currently highlighted vertices
			for (AbstractLayoutVertex v : this.highlighted) {
				highlighted.remove(v);
				v.setHighlighted(false);
			}
		}

		if(addChosen) {
			//Next we add the highlighted vertices
			HashSet<AbstractLayoutVertex> toAddHighlights = panelRoot.getVerticesWithInstructionID(index, method);
			for (AbstractLayoutVertex v : toAddHighlights) {
				highlighted.add(v);
				v.setHighlighted(true);
			}
		} else {
			HashSet<AbstractLayoutVertex> toRemoveHighlights = panelRoot.getVerticesWithInstructionID(index, method);
			for(AbstractLayoutVertex v : toRemoveHighlights) {
				highlighted.remove(v);
				v.setHighlighted(false);
			}
		}
	}

	public void resetHighlighted(AbstractLayoutVertex newHighlighted)
	{
		for(AbstractLayoutVertex currHighlighted : this.highlighted)
			currHighlighted.setHighlighted(false);
		highlighted = new HashSet<AbstractLayoutVertex>();

		if(newHighlighted != null) {
			highlighted.add(newHighlighted);
			newHighlighted.setHighlighted(true);
		}
	}

	public void drawNodes(GUINode parent, AbstractLayoutVertex v)
	{
		GUINode node = new GUINode(parent, v);

		if (parent == null){
			contentGroup.getChildren().add(node);
		}
		else{
			parent.getChildren().add(node);
		}

		double translateX = scaleX(v.getX());
		double translateY = scaleY(v.getY());
		double width = scaleX(v.getWidth());
		double height = scaleY(v.getHeight());
		node.setLocation(translateX, translateY, width, height);

		// Move these to initialization?
		node.setArcWidth(scaleX(0.5));
		node.setArcHeight(scaleY(0.5));
		//node.setLabel("  " + v.getLabel());

		node.setFill(v.getColor());
		node.setStroke(javafx.scene.paint.Color.BLACK);
		node.setStrokeWidth(.5);
		node.setOpacity(1);

		if (v.getInnerGraph().getVertices().size() == 0)
			return;

		Iterator<AbstractLayoutVertex> it = v.getInnerGraph().getVertices().values().iterator();
		while (it.hasNext())
		{
			AbstractLayoutVertex child = it.next();
			if(v.isExpanded()){
				drawNodes(node, child);
			}
		}
	}

	public void drawEdges(GUINode parent, AbstractLayoutVertex v)
	{
		//System.out.println("Edges of vertex: " + v.getStrID());
		if(!Parameters.edgeVisible){
			return;
		}
		
		GUINode node = v.getGraphics();
		if(v.isExpanded())
		{
			//Edge.arrowLength = this.getWidthPerVertex() / 10.0;
			for(LayoutEdge e : v.getInnerGraph().getEdges().values())
				e.draw(this, node);
		
			for(AbstractLayoutVertex child : v.getInnerGraph().getVertices().values())
				drawEdges(node, child);
			
		}
	}

	/*public static void computeHues()
	{
		float start = 0.4f; //green
		float end = 0.0f; //red

		VizPanel.maxLoopHeight = 0;
		for(Vertex v : Main.graph.vertices)
		{
			if(v.loopHeight > maxLoopHeight)
				maxLoopHeight = v.loopHeight;
		}
		
		System.out.println("Max loop height: " + maxLoopHeight);
		
		hues = new float[maxLoopHeight + 1];
		for(int i = 0; i <= maxLoopHeight; i++)
		{
			// Linear interpolation of color values
			hues[i] = start - ((float) i)/(maxLoopHeight + 1)*(start - end);
		}
	}*/

	// Next three methods copied from solution here: https://community.oracle.com/thread/2541811
	// Feature request (inactive) to have an easier way to zoom inside a ScrollPane:
	// https://bugs.openjdk.java.net/browse/JDK-8091618
	private ScrollPane createZoomPane(final Group group)
	{
		final double SCALE_DELTA = 1.1;
		final StackPane zoomPane = new StackPane();

		zoomPane.getChildren().add(group);

		final ScrollPane scroller = new ScrollPane();
		final Group scrollContent = new Group(zoomPane);
		scroller.setContent(scrollContent);

		scroller.viewportBoundsProperty().addListener(new ChangeListener<Bounds>()
		{
			@Override
			public void changed(ObservableValue<? extends Bounds> observable,
								Bounds oldValue, Bounds newValue)
			{
				zoomPane.setMinSize(newValue.getWidth(), newValue.getHeight());
			}
		});

		
		final EventHandler<ScrollEvent> voindHandle =  new EventHandler<ScrollEvent>()
		{
			@Override
			public void handle(ScrollEvent event)
			{
				event.consume();
				System.out.println("voindHandle");
			}
		}; 
		

		EventHandler<ScrollEvent> activeHandle = new EventHandler<ScrollEvent>()
		{
			@Override
			public void handle(ScrollEvent event)
			{
				event.consume();
				System.out.println("ZOOOOOOOOOOOM");
				zoomPane.setOnScroll(voindHandle);
				

				if (event.getDeltaY() == 0)
					return;

				final double scaleFactor = (event.getDeltaY() > 0) ? SCALE_DELTA
						: 1 / SCALE_DELTA;

				// amount of scrolling in each direction in scrollContent coordinate units
				final Point2D scrollOffset = figureScrollOffset(scrollContent, scroller);

				ScaleTransition st = new ScaleTransition(Duration.millis(5),group);
				st.setToX(group.getScaleX()*scaleFactor);
				st.setToY(group.getScaleX()*scaleFactor);
//				group.setScaleX(group.getScaleX() * scaleFactor);
//				group.setScaleY(group.getScaleY() * scaleFactor);
				Parameters.stFrame.mainPanel.getPanelRoot().toggleEdges();
				st.play();
				
				final EventHandler<ScrollEvent> activeHandle = this;
				
				st.setOnFinished(new EventHandler<ActionEvent>() {
					@Override
					public void handle(ActionEvent event)
					{

						// move viewport so that old center remains in the center after the scaling
						repositionScroller(scrollContent, scroller, scaleFactor, scrollOffset);
						Parameters.stFrame.mainPanel.getPanelRoot().toggleEdges();
						// Adjust stroke width of lines and length of arrows
						VizPanel.this.scaleLines();
						zoomPane.setOnScroll(activeHandle);
					}
				});
			}
		}; 
		
		
		zoomPane.setOnScroll(activeHandle);

		// Panning via drag....
		final ObjectProperty<Point2D> lastMouseCoordinates = new SimpleObjectProperty<Point2D>();
		scrollContent.setOnMousePressed(new EventHandler<MouseEvent>() {
			@Override
			public void handle(MouseEvent event) {
				lastMouseCoordinates.set(new Point2D(event.getX(), event.getY()));
			}
		});

		// Fix drag location when node is scaled
		scrollContent.setOnMouseDragged(new EventHandler<MouseEvent>()
		{
			@Override
			public void handle(MouseEvent event)
			{
				double deltaX = event.getX() - lastMouseCoordinates.get().getX();
				double extraWidth = scrollContent.getLayoutBounds().getWidth() - scroller.getViewportBounds().getWidth();
				double deltaH = deltaX * (scroller.getHmax() - scroller.getHmin()) / extraWidth;
				double desiredH = scroller.getHvalue() - deltaH;
				scroller.setHvalue(Math.max(0, Math.min(scroller.getHmax(), desiredH)));

				double deltaY = event.getY() - lastMouseCoordinates.get().getY();
				double extraHeight = scrollContent.getLayoutBounds().getHeight() - scroller.getViewportBounds().getHeight();
				double deltaV = deltaY * (scroller.getHmax() - scroller.getHmin()) / extraHeight;
				double desiredV = scroller.getVvalue() - deltaV;
				scroller.setVvalue(Math.max(0, Math.min(scroller.getVmax(), desiredV)));
			}
		});

		return scroller;
	}

	private Point2D figureScrollOffset(Node scrollContent, ScrollPane scroller)
	{
		double extraWidth = scrollContent.getLayoutBounds().getWidth() - scroller.getViewportBounds().getWidth();
		double hScrollProportion = (scroller.getHvalue() - scroller.getHmin()) / (scroller.getHmax() - scroller.getHmin());
		double scrollXOffset = hScrollProportion * Math.max(0, extraWidth);
		double extraHeight = scrollContent.getLayoutBounds().getHeight() - scroller.getViewportBounds().getHeight();
		double vScrollProportion = (scroller.getVvalue() - scroller.getVmin()) / (scroller.getVmax() - scroller.getVmin());
		double scrollYOffset = vScrollProportion * Math.max(0, extraHeight);
		return new Point2D(scrollXOffset, scrollYOffset);
	}

	private void repositionScroller(Node scrollContent, ScrollPane scroller, double scaleFactor, Point2D scrollOffset)
	{
		double scrollXOffset = scrollOffset.getX();
		double scrollYOffset = scrollOffset.getY();
		double extraWidth = scrollContent.getLayoutBounds().getWidth() - scroller.getViewportBounds().getWidth();
		if (extraWidth > 0)
		{
			double halfWidth = scroller.getViewportBounds().getWidth() / 2 ;
			double newScrollXOffset = (scaleFactor - 1) *  halfWidth + scaleFactor * scrollXOffset;
			scroller.setHvalue(scroller.getHmin() + newScrollXOffset * (scroller.getHmax() - scroller.getHmin()) / extraWidth);
		}
		else
		{
			scroller.setHvalue(scroller.getHmin());
		}

		double extraHeight = scrollContent.getLayoutBounds().getHeight() - scroller.getViewportBounds().getHeight();
		if (extraHeight > 0)
		{
			double halfHeight = scroller.getViewportBounds().getHeight() / 2 ;
			double newScrollYOffset = (scaleFactor - 1) * halfHeight + scaleFactor * scrollYOffset;
			scroller.setVvalue(scroller.getVmin() + newScrollYOffset * (scroller.getVmax() - scroller.getVmin()) / extraHeight);
		}
		else
		{
			scroller.setHvalue(scroller.getHmin());
		}
	}

	public void scaleLines()
	{
		//System.out.println("Scaling lines and arrowheads...");
		for(LayoutEdge e : this.panelRoot.getInnerGraph().getEdges().values())
			e.setScale();

		for(AbstractLayoutVertex v : this.panelRoot.getInnerGraph().getVertices().values())
		{
			for(LayoutEdge e : v.getInnerGraph().getEdges().values())
				e.setScale();
		}
	}

	public void incrementScaleXFactor() {
		factorX *= factorMultiple;
	}
	
	public void decrementScaleXFactor() {
		factorX /= factorMultiple;
	}
	
	public void incrementScaleYFactor() {
		factorY *= factorMultiple;
	}
	
	public void decrementScaleYFactor() {
		factorY /= factorMultiple;
	}
}