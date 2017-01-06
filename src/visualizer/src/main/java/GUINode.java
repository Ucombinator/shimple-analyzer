
import javafx.scene.layout.Pane;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Text;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;

// TODO: Place vertex labels on top of vertices.
public class GUINode extends Pane
{
    double dragX, dragY;
    protected Rectangle rect;
    protected Text rectLabel;
    private AbstractVertex vertex;
	private GUINode parent;

    boolean labels_enabled = false;
    boolean isDragging;

    public GUINode(GUINode parent, AbstractVertex v)
    {
        super();
        this.parent = parent;
        this.vertex = v;
        this.vertex.setGraphics(this);
        this.rect = new Rectangle();
        this.rectLabel = new Text();
        if(labels_enabled)
        {
        	this.getChildren().addAll(this.rect, this.rectLabel);
        }
        else
        {
        	this.getChildren().addAll(this.rect);
        }

        this.rect.setOpacity(0.2);
        this.makeDraggable();
        this.isDragging = false;
        
        this.setOnMouseClicked(new AnimationHandler());
        this.setVisible(true);
    }
    
    public AbstractVertex getVertex() {
		return vertex;
	}

	public void setVertex(AbstractVertex vertex) {
		this.vertex = vertex;
	}

    public String toString()
    {
        return rectLabel.getText().toString();
    }

    public void setLabel(String text)
    {
        this.rectLabel.setText(text);
    }

    // Next several methods: Pass on calls to underlying rectangle
    public void setFill(Color c)
    {
        this.rect.setFill(c);
    }

    public void setStroke(Color c)
    {
        this.rect.setStroke(c);
    }

    public void setStrokeWidth(double strokeWidth)
    {
        this.rect.setStrokeWidth(strokeWidth);
    }

    public void setArcHeight(double height)
    {
        this.rect.setArcHeight(height);
    }

    public void setArcWidth(double height)
    {
        this.rect.setArcWidth(height);
    }

    public void setLocation(double x, double y, double width, double height)
    {
        this.setTranslateX(x);
        this.setTranslateY(y);
        this.rect.setWidth(width);
        this.rect.setHeight(height);
    }

    public void makeDraggable()
    {
        this.setOnMousePressed(onMousePressedEventHandler);
        this.setOnMouseDragged(onMouseDraggedEventHandler);
        this.setOnMouseReleased(onMouseReleasedEventHandler);
        this.setOnMouseEntered(onMouseEnteredEventHandler);
        this.setOnMouseExited(onMouseExitedEventHandler);
    }

    EventHandler<MouseEvent> onMousePressedEventHandler = new EventHandler<MouseEvent>()
    {
        @Override
        public void handle(MouseEvent event)
        {
            event.consume();
            GUINode node = (GUINode) event.getSource();

            dragX = node.getBoundsInParent().getMinX() - event.getScreenX();
            dragY = node.getBoundsInParent().getMinY() - event.getScreenY();
        }
    };

    EventHandler<MouseEvent> onMouseDraggedEventHandler = new EventHandler<MouseEvent>()
    {
        @Override
        public void handle(MouseEvent event)
        {
            event.consume();
            GUINode node = (GUINode) event.getSource();

            node.isDragging = true;
            double offsetX = event.getScreenX() + dragX;
            double offsetY = event.getScreenY() + dragY;
            node.setTranslateX(offsetX);
            node.setTranslateY(offsetY);
        }
    };

    EventHandler<MouseEvent> onMouseReleasedEventHandler = new EventHandler<MouseEvent>()
    {
        @Override
        public void handle(MouseEvent event)
        {
            event.consume();
            GUINode node = (GUINode) event.getSource();

            if (node.isDragging)
            {
                node.isDragging = false;
            }
        }
    };

    EventHandler onMouseEnteredEventHandler = new javafx.event.EventHandler()
    {
        @Override
        public void handle(Event event)
        {
            event.consume();
        	if (vertex.getSelfGraph()!=null)
        	{
	        	for(Edge e : vertex.getSelfGraph().getEdges().values())
                {
	        		if(e.getSourceVertex() == vertex || e.getDestVertex() == vertex)
	        		{
	        		    e.getLine().setStroke(Color.GREENYELLOW);
	        		    e.getLine().setStrokeWidth(2);
	        		}
	        	}
        	}
        	
            GUINode obj = (GUINode) (event.getSource());
            obj.rect.setOpacity(1);
            if (obj.parent != null)
                obj.parent.rect.setOpacity(0.3);
        }
    };

	EventHandler onMouseExitedEventHandler = new javafx.event.EventHandler()
    {
        @Override
        public void handle(Event event)
        {
            event.consume();
        	if(vertex.getSelfGraph() != null)
        	{
	        	for(Edge e : vertex.getSelfGraph().getEdges().values())
                {
	        		if (e.getSourceVertex() == vertex || e.getDestVertex() == vertex)
	        		{
	        			e.getLine().setStroke(Color.BLACK);
	        			e.getLine().setStrokeWidth(1);
	        		}
	        	}
        	}
        	
            GUINode obj = (GUINode) (event.getSource());
            obj.rect.setOpacity(0.3);
            if(obj.parent != null)
                obj.parent.rect.setOpacity(1);
        }
    };
}
