package org.ucombinator.jaam.visualizer.gui;

import com.sun.org.apache.regexp.internal.RE;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.property.DoubleProperty;
import javafx.geometry.BoundingBox;
import javafx.geometry.Bounds;
import javafx.geometry.Point2D;
import javafx.scene.Group;
import javafx.scene.effect.Effect;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Line;
import javafx.scene.shape.Polygon;
import javafx.scene.shape.QuadCurve;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Text;
import javafx.scene.paint.Color;
import org.ucombinator.jaam.visualizer.layout.*;
import org.ucombinator.jaam.visualizer.state.StateRootVertex;
import org.ucombinator.jaam.visualizer.taint.TaintRootVertex;

public class GUINode extends Group {

    protected final Rectangle rect;
    protected final AbstractLayoutVertex vertex;
    protected final GUINode parent;

    private Point2D dragStart;

    public GUINode(GUINode parent, AbstractLayoutVertex v)
    {
        super();
        this.parent = parent;
        this.vertex = v;

        this.rect = new Rectangle();

        this.getChildren().add(this.rect);

        this.rect.setArcWidth(5);
        this.rect.setArcHeight(5);

        this.setFill(v.getFill());
        this.rect.setStroke(Color.BLACK);
        this.setStrokeWidth(0.5);
        this.setOpacity(1);

        this.setOnMousePressed(this::handleOnMousePressed);
        this.setOnMouseDragged(this::handleOnMouseDragged);
        this.setOnMouseEntered(this::handleOnMouseEntered);
        this.setOnMouseExited(this::handleOnMouseExited);
        this.setOnMouseClicked(this::handleOnMouseClicked);
        this.setVisible(true);
    }

    private void handleOnMousePressed(MouseEvent event) {
        event.consume();
        this.dragStart = new Point2D(event.getX(), event.getY());
    }

    private void handleOnMouseDragged(MouseEvent event) {
        event.consume();

        double newX = this.getTranslateX() + event.getX() - this.dragStart.getX();
        double newY = this.getTranslateY() + event.getY() - this.dragStart.getY();

        // Clamp the offset to confine our box to its parent.
        if (this.getParentNode() != null) {

            Bounds thisBounds = this.getRectBoundsInLocal();
            Bounds parentBounds = this.getParentNode().getRectBoundsInLocal();

            newX = Math.min(Math.max(newX, 0), parentBounds.getWidth() - thisBounds.getWidth());
            newY = Math.min(Math.max(newY, 0), parentBounds.getHeight() - thisBounds.getHeight());
        }

        this.setTranslateX(newX);
        this.setTranslateY(newY);

        this.vertex.setX(newX);
        this.vertex.setY(newY);

        this.vertex.getIncidentEdges().forEach(LayoutEdge::redrawEdge);
    }

    public double getWidth() {
        return getWidthProperty().get();
    }

    public DoubleProperty getWidthProperty() {
        return rect.widthProperty();
    }

    private void handleOnMouseEntered(MouseEvent event) {
        event.consume();
        this.vertex.onMouseEnter();
    }

    private void handleOnMouseExited(MouseEvent event) {
        event.consume();
        this.vertex.onMouseExit();
    }

    private void handleOnMouseClicked(MouseEvent event) {
        event.consume();
        this.vertex.onMouseClick(event);
    }

    public AbstractLayoutVertex getVertex() {
        return vertex;
    }

    public String toString() {
        return "GUI_NODE(" + vertex + ")";
    }

    public void setFill(Paint c) {
        this.rect.setFill(c);
    }

    public void setStrokeWidth(double strokeWidth) {
        this.rect.setStrokeWidth(strokeWidth);
    }

    public void setTranslateLocation(double x, double y, double width, double height) {
        this.setTranslateX(x);
        this.setTranslateY(y);

        this.rect.setWidth(width);
        this.rect.setHeight(height);
    }

    // Returns the bounding box for just the rectangle in the coordinate system for the parent of our node.
    public Bounds getRectBoundsInParent() {
        return this.getUnaffectedBoundsInParent();
        /*
        Bounds nodeBounds = this.getUnaffectedBoundsInParent();
        Bounds nodeBoundsLocal = this.getBoundsInLocal();
        Bounds rectBounds = this.getUnaffectedRectBoundsInParent();

        Bounds result = new BoundingBox(nodeBounds.getMinX() + rectBounds.getMinX() - nodeBoundsLocal.getMinX(),
                nodeBounds.getMinY() + rectBounds.getMinY() - nodeBoundsLocal.getMinY(),
                rectBounds.getWidth(), rectBounds.getHeight());

        System.out.println("Getting bounds\n\tnode" + nodeBounds + "\n\trect " + rectBounds + "\n\tres " + result);

        return result;
        */
    }

    private Bounds getUnaffectedBoundsInParent() {

        if (this.getEffect() == null) {
            return this.getBoundsInParent();
        }

        Effect effect = this.getEffect();
        this.setEffect(null);

        Bounds result = this.getBoundsInParent();
        this.setEffect(effect);

        return result;
    }

    public void printLocation() {
        Bounds bounds = this.getBoundsInParent();
        System.out.println("Node x = " + bounds.getMinX() + ", " + bounds.getMaxX());
        System.out.println("Node y = " + bounds.getMinY() + ", " + bounds.getMaxY());
    }

    public static <T extends AbstractLayoutVertex> Line getLine(GUINode sourceNode, GUINode destNode) {
        if(sourceNode == null || destNode == null) {
            System.out.println("This should never happen!");
            return new Line(0, 0, 0, 0);
        }
        else {
            /*
            System.out.println("Get line: " + sourceNode.getVertex() + " --> " + destNode.getVertex());
            System.out.println("\tBounds:\n\tS:\t" + sourceNode.getBoundsInParent() + "\n\tD:\t" + destNode.getBoundsInParent());
            System.out.println("\tIntersections\n\tS:\t" + sourceNode.getLineIntersection(destNode) + "\n\tD:\t" + destNode.getLineIntersection(sourceNode));
            */

            Point2D sourceIntersect = sourceNode.getLineIntersection(destNode);
            Point2D destIntersect = destNode.getLineIntersection(sourceNode);

            return new Line(sourceIntersect.getX(), sourceIntersect.getY(),
                    destIntersect.getX(), destIntersect.getY());
        }
    }

    public static <T extends AbstractLayoutVertex> QuadCurve getCurve(GUINode sourceNode, GUINode destNode) {
        if(sourceNode == null || destNode == null) {
            System.out.println("This should never happen!");
            return new QuadCurve(0, 0, 0, 0, 0, 0);
        }
        else {
            Point2D sourceIntersect = sourceNode.getLineIntersection(destNode);
            Point2D destIntersect = destNode.getLineIntersection(sourceNode);
            Point2D controlPoint = getControlPoint(sourceIntersect, destIntersect);
            return new QuadCurve(sourceIntersect.getX(), sourceIntersect.getY(),
                    controlPoint.getX(), controlPoint.getY(), destIntersect.getX(), destIntersect.getY());
        }
    }

    private static Point2D getControlPoint(Point2D p1, Point2D p2) {
        double frac = 0.8;
        return new Point2D((1 - frac) * p1.getX() + frac * p2.getX(),
                frac * p1.getY() + (1 - frac) * p2.getY());
    }

    private Point2D getLineIntersection(GUINode otherNode) {
        Bounds sourceBounds = this.getRectBoundsInParent();
        Bounds destBounds = otherNode.getRectBoundsInParent();

        double sourceCenterX = (sourceBounds.getMinX() + sourceBounds.getMaxX()) / 2.0;
        double sourceCenterY = (sourceBounds.getMinY() + sourceBounds.getMaxY()) / 2.0;
        double destCenterX = (destBounds.getMinX() + destBounds.getMaxX()) / 2.0;
        double destCenterY = (destBounds.getMinY() + destBounds.getMaxY()) / 2.0;
        double sourceExitX, sourceExitY;

        // To find which side a line exits from, we compute both diagonals of the rectangle and
        // determine whether the other end lies above or below each diagonal. The positive diagonal
        // uses the positive slope, and the negative diagonal uses the negative slope.
        // Keep in mind that the increasing y direction is downward.
        double startDiagSlope = sourceBounds.getHeight() / sourceBounds.getWidth();
        double startInterceptPos = sourceCenterY - sourceCenterX * startDiagSlope;
        double startInterceptNeg = sourceCenterY + sourceCenterX * startDiagSlope;
        boolean aboveStartPosDiag = (destCenterX * startDiagSlope + startInterceptPos > destCenterY);
        boolean aboveStartNegDiag = (-destCenterX * startDiagSlope + startInterceptNeg > destCenterY);

        if (aboveStartPosDiag && aboveStartNegDiag)
        {
            // Top
            double invSlope = (destCenterX - sourceCenterX) / (destCenterY - sourceCenterY);
            sourceExitY = sourceBounds.getMinY();
            sourceExitX = sourceCenterX + invSlope * (sourceExitY - sourceCenterY);
        }
        else if (!aboveStartPosDiag && aboveStartNegDiag)
        {
            // Left
            double slope = (destCenterY - sourceCenterY) / (destCenterX - sourceCenterX);
            sourceExitX = sourceBounds.getMinX();
            sourceExitY = sourceCenterY + slope * (sourceExitX - sourceCenterX);
        }
        else if (aboveStartPosDiag && !aboveStartNegDiag)
        {
            // Right
            double slope = (destCenterY - sourceCenterY) / (destCenterX - sourceCenterX);
            sourceExitX = sourceBounds.getMaxX();
            sourceExitY = sourceCenterY + slope * (sourceExitX - sourceCenterX);
        }
        else
        {
            // Bottom
            double invSlope = (destCenterX - sourceCenterX) / (destCenterY - sourceCenterY);
            sourceExitY = sourceBounds.getMaxY();
            sourceExitX = sourceCenterX + invSlope * (sourceExitY - sourceCenterY);
        }

        return new Point2D(sourceExitX, sourceExitY);
    }

    public static Polygon computeArrowhead(double xTip, double yTip, double length, double orientAngle, double angleWidth) {
        double x1 = xTip + length * Math.cos(orientAngle + angleWidth);
        double y1 = yTip + length * Math.sin(orientAngle + angleWidth);
        double x2 = xTip + length * Math.cos(orientAngle - angleWidth);
        double y2 = yTip + length * Math.sin(orientAngle - angleWidth);

        Polygon arrowhead = new Polygon();
        arrowhead.getPoints().addAll(
                xTip, yTip,
                x1, y1,
                x2, y2
        );
        return arrowhead;
    }

    public GUINode getParentNode() {
        return this.parent;
    }

    public Bounds getRectBoundsInLocal() {
        if (this.rect.getEffect() == null) {
            return this.rect.getBoundsInLocal();
        }
        else {
            final Effect effect = this.rect.getEffect();
            this.rect.setEffect(null);
            Bounds result = this.rect.getBoundsInLocal();
            this.rect.setEffect(effect);
            return result;
        }
    }

    public Bounds getUnaffectedRectBoundsInParent() {
        if(this.rect.getEffect() == null) {
            return this.rect.getBoundsInParent();
        }
        else {
            final Effect effect = this.rect.getEffect();
            this.rect.setEffect(null);
            Bounds result = this.rect.getBoundsInParent();
            this.rect.setEffect(effect);
            return result;
        }
    }
}
