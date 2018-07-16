package org.ucombinator.jaam.visualizer.controllers;

import com.strobel.decompiler.languages.java.ast.CompilationUnit;
import javafx.beans.property.SimpleSetProperty;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import org.ucombinator.jaam.visualizer.classTree.ClassTreeNode;
import org.ucombinator.jaam.visualizer.classTree.ClassTreeUtils;
import org.ucombinator.jaam.visualizer.graph.Graph;
import org.ucombinator.jaam.visualizer.layout.AbstractLayoutVertex;
import org.ucombinator.jaam.visualizer.state.*;
import org.ucombinator.jaam.visualizer.taint.*;
import soot.SootClass;

import java.io.File;
import java.io.IOException;
import java.util.*;

public class MainTabController {
    public final Tab tab;
    public final VizPanelController vizPanelController;
    public final TaintPanelController taintPanelController;
    public final CodeViewController codeViewController;
    public final SearchResultsController searchResultsController;

    // Left Side Components
    @FXML private final VBox leftPane = null; // Initialized by Controllers.loadFXML()

    // Center Components
    @FXML private final Node root = null; // Initialized by Controllers.loadFXML()
    @FXML private final BorderPane vizPane = null; // Initialized by Controllers.loadFXML()
    @FXML private final BorderPane taintPane = null; // Initialized by Controllers.loadFXML()

    // Right Side Components
    @FXML private final TextArea vizDescriptionArea = null; // Initialized by Controllers.loadFXML()
    @FXML private final TextArea taintDescriptionArea = null; // Initialized by Controllers.loadFXML()
    @FXML private TreeView<ClassTreeNode> classTree = null; // Initialized by Controllers.loadFXML()
    @FXML private final BorderPane searchPane = null; // Initialized by Controllers.loadFXML()

    private HashSet<StateVertex> vizHighlighted; // TODO: Make this an observable set
    private HashSet<TaintVertex> taintHighlighted;
    private SetHistoryProperty<StateVertex> hidden;

    public MainTabController(File file, Graph<StateVertex, StateEdge> graph, List<CompilationUnit> compilationUnits,
                             Graph<TaintVertex, TaintEdge> taintGraph, Set<SootClass> sootClasses) throws IOException {
        Controllers.loadFXML("/MainTabContent.fxml", this);

        this.codeViewController = new CodeViewController(compilationUnits, sootClasses);
        this.leftPane.getChildren().add(this.codeViewController.codeTabs);

        this.vizPanelController = new VizPanelController(graph);
        this.vizPane.setCenter(this.vizPanelController.root);

        this.taintPanelController = new TaintPanelController(taintGraph, this.codeViewController);
        this.taintPane.setCenter(this.taintPanelController.root);

        this.searchResultsController = new SearchResultsController();
        this.searchPane.setCenter(this.searchResultsController.root);

        this.tab = new Tab(file.getName(), this.root);
        this.tab.tooltipProperty().set(new Tooltip(file.getAbsolutePath()));
        Controllers.put(this.tab, this);

        this.codeViewController.addSelectHandler(vizPane);
        this.taintPanelController.addSelectHandler(vizPane);

        ClassTreeUtils.buildClassTree(this.classTree, this.codeViewController, this.vizPanelController.getImmutableRoot());

        this.vizHighlighted = new LinkedHashSet<>();
        this.taintHighlighted = new LinkedHashSet<>();
        this.hidden = new SetHistoryProperty<>(new SimpleSetProperty<>(FXCollections.observableSet()));
        this.hidden.addListener(this.vizPanelController);
    }

    public TreeView<ClassTreeNode> getClassTree() {
        return this.classTree;
    }

    public void setRightText(StateLoopVertex v)
    {
        this.vizDescriptionArea.setText("Loop:\n  Class: "
                + v.getClassDeclaration() + "\n  Method: "
                + v.getMethodName()       + "\n  Index: "
                + v.getStatementIndex()   + "\n  Signature: " + v.getLabel()
                + "\n  Loop info: " + v.getCompilationUnit().loopInfo() + "\n");
    }


    public void setRightText(StateMethodVertex v)
    {
        this.vizDescriptionArea.setText("Method:\n  Class: "
                + v.getClassDeclaration() + "\n  Method: "
                + v.getMethodName()       + "\n  Signature: " + v.getLabel());
    }

    public void setRightText(StateSccVertex v)
    {
        StringBuilder text = new StringBuilder("SCC contains:\n");
        int k = 0;
        Graph<StateVertex, StateEdge> childGraph = v.getInnerGraph();
        for (StateVertex i : childGraph.getVertices()) {
            text.append(k++ + "  " + i.getLabel() + "\n");
        }
        this.vizDescriptionArea.setText(text.toString());
    }

    public void setRightText(TaintAddress v) {
        this.taintDescriptionArea.setText("Taint address:\n" + v.toString());
    }

    public void setRightText(TaintSccVertex v) {
        StringBuilder text = new StringBuilder("SCC contains:\n");
        int k = 0;
        for(AbstractLayoutVertex<TaintVertex> i : v.getLineSortedChildren()) {
            text.append(k++ + "  " + i.getLabel() + "\n");
        }
        this.taintDescriptionArea.setText(text.toString());
    }

    public void setRightText(TaintStmtVertex v) {
        StringBuilder text = new StringBuilder("Statement: " + v.getStmt());
        text.append("\nAddresses: " + v.getAddresses().size());
        this.taintDescriptionArea.setText(text.toString());
    }

    public void setRightText(TaintMethodVertex v) {
        this.taintDescriptionArea.setText(v.getRightPanelString());
    }

    public void setVizRightText(String text) {
        this.vizDescriptionArea.setText(text);
    }

    public void setTaintRightText(String text) {
        this.taintDescriptionArea.setText(text);
    }

    public SetHistoryProperty<StateVertex> getHidden() {
        return this.hidden;
    }

    public void hideSelectedNodes() {
        this.hidden.addAll(this.getVizHighlighted());
        this.vizHighlighted.clear();
    }

    /*
    public void pruneVisibleGraph() {
        HashSet<StateVertex> prunedVertices = this.vizPanelController.pruneVisibleGraph();
        this.hidden.addAll(prunedVertices);
        this.vizHighlighted.removeAll(prunedVertices);
        this.vizPanelController.redrawGraph(this.hidden);
    }
    */

    public void showAllNodes() {
        this.hidden.clear();
        this.vizPanelController.redrawGraph(this.hidden);
    }

    public HashSet<StateVertex> getVizHighlighted() {
        return this.vizHighlighted;
    }

    public HashSet<TaintVertex> getTaintHighlighted() {
        return this.taintHighlighted;
    }

    public void addToHighlighted(StateVertex v) {
        if(v != null) {
            vizHighlighted.add(v);
            v.setHighlighted(true);
        }
    }

    public void resetVizHighlighted() {
        for(StateVertex currHighlighted : vizHighlighted) {
            currHighlighted.setHighlighted(false);
        }
        vizHighlighted.clear();
    }

    public void resetHighlighted(StateVertex newHighlighted) {
        resetVizHighlighted();
        addToHighlighted(newHighlighted);
    }

    public void addToHighlighted(TaintVertex v) {
        if(v != null) {
            taintHighlighted.add(v);
            v.setHighlighted(true);
        }
    }

    public void resetTaintHighlighted() {
        for(TaintVertex currHighlighted : taintHighlighted) {
            currHighlighted.setHighlighted(false);
        }
        taintHighlighted.clear();
    }

    public void resetHighlighted(TaintVertex newHighlighted) {
        resetTaintHighlighted();
        addToHighlighted(newHighlighted);
    }

    public void hideUnrelatedToHighlighted() {
        if (this.vizHighlighted.isEmpty()) { return; }

        HashSet<StateVertex> toHide = vizPanelController.getUnrelatedVisible(this.vizHighlighted);

        this.hidden.addAll(vizPanelController.getImmutable(toHide));
        this.vizHighlighted.clear();
        this.vizPanelController.redrawGraph(this.hidden);
    }

    // TODO: This should be done using event and event handling using FieldSelectEvent
    public void selectFieldInTaintGraph(String fullClassName, String fieldName) {
        taintPanelController.showFieldTaintGraph(fullClassName, fieldName);
    }

    public void setClassHighlight(HashSet<StateVertex> vertices, boolean value) {
        vizPanelController.setClassHighlight(vertices, value);
    }
}
