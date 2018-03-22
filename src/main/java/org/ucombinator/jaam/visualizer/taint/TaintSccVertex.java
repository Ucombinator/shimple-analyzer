package org.ucombinator.jaam.visualizer.taint;

import javafx.scene.paint.Color;

import java.util.Collection;
import java.util.HashSet;

public class TaintSccVertex extends TaintVertex {

    private Color defaultColor = Color.DARKGREY;

    public TaintSccVertex(String label)
    {
        super(label, VertexType.SCC, true);
        this.color = defaultColor;
    }

    public TaintSccVertex copy() {
        return new TaintSccVertex(this.getLabel());
    }

    public HashSet<String> getMethodNames() {
        HashSet<String> methodNames = new HashSet<>();
        for (TaintVertex v : this.getChildGraph().getVertices()) {
            methodNames.addAll(v.getMethodNames());
        }
        return methodNames;
    }

    @Override
    public boolean hasField() {
        for (TaintVertex v : this.getChildGraph().getVertices()) {
            if (v.hasField()) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void getFields(Collection<TaintAddress> store) {
        this.getChildGraph().getVertices().forEach(v -> v.getFields(store));
    }

    @Override
    public String getStmtString() {
        return null;
    }
}
