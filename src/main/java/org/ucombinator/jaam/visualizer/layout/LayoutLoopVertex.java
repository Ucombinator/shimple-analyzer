package org.ucombinator.jaam.visualizer.layout;

import org.ucombinator.jaam.visualizer.gui.GUINode;
import org.ucombinator.jaam.visualizer.gui.VizPanel;

import java.util.HashSet;

/**
 * Created by timothyjohnson on 5/4/17.
 */
public class LayoutLoopVertex extends AbstractLayoutVertex {

    private int startJimpleIndex;
    private LayoutMethodVertex methodVertex;

    public LayoutLoopVertex(LayoutMethodVertex methodVertex, int startIndex, boolean drawEdges) {
        super(methodVertex.getMethodName() + ", instruction #" + startIndex, VertexType.LOOP, drawEdges);
        this.methodVertex = methodVertex;
        this.startJimpleIndex = startIndex;
    }

    public String getMethodName() {
        return this.methodVertex.getMethodName();
    }

    public String getRightPanelContent() {
        return "Method vertex: " + this.getMethodName() + "\nLoop height: " + this.getLoopHeight() + "\n";
    }

    public String getShortDescription() {
        return this.getMethodName();
    }

    public GUINode.ShapeType getShape() {
        return GUINode.ShapeType.RECTANGLE;
    }

    public boolean searchByMethod(String query, VizPanel mainPanel) {
        boolean found = this.methodVertex.getMethodName().contains(query);
        if(found)
            this.setHighlighted(found, mainPanel);

        return found;
    }

    public HashSet<LayoutMethodVertex> getMethodVertices() {
        HashSet<LayoutMethodVertex> methods = new HashSet<LayoutMethodVertex>();
        methods.add(this.methodVertex);
        return methods;
    }
}