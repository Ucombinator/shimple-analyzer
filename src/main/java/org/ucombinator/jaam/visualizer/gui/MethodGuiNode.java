package org.ucombinator.jaam.visualizer.gui;

import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import org.ucombinator.jaam.visualizer.taint.TaintMethodVertex;
import org.ucombinator.jaam.visualizer.taint.TaintVertex;

import java.util.ArrayList;

public class MethodGuiNode extends GUINode{

    BorderPane pane;
    HBox inputPane, outPane;
    Label centerLabel;

    ArrayList<TaintVertex> inputs;
    ArrayList<TaintVertex> outputs;

    public MethodGuiNode(GUINode parent, TaintMethodVertex v) {
        super(parent, v);

        pane = new BorderPane();
        inputPane = new HBox();
        outPane = new HBox();
        fillTopAndBottom();
        centerLabel = new Label(v.getMethodName());
    }

    private void fillTopAndBottom() {
        for (TaintVertex v : inputs) {
            inputPane.getChildren().add(v.getGraphics());
        }
        for (TaintVertex v : outputs) {
            outPane.getChildren().add(v.getGraphics());
        }
    }

    private TaintMethodVertex getV() {
        return (TaintMethodVertex) vertex;
    }
}
