package org.ucombinator.jaam.visualizer.controllers;

import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import org.ucombinator.jaam.visualizer.graph.Graph;
import org.ucombinator.jaam.visualizer.gui.*;
import org.ucombinator.jaam.visualizer.layout.AbstractLayoutVertex;

import java.io.File;
import java.io.IOException;

public class MainTabController {
    public final Tab tab;
    public final VizPanelController vizPanelController;

    // TODO: rename some of these
    @FXML public final CodeArea bytecodeArea = null; // Initialized by Controllers.loadFXML()

    @FXML private final Node root = null; // Initialized by Controllers.loadFXML()
    @FXML private final BorderPane centerPane = null; // Initialized by Controllers.loadFXML()
    @FXML private final TextArea descriptionArea = null; // Initialized by Controllers.loadFXML()
    @FXML private final SearchResults searchResults = null; // Initialized by Controllers.loadFXML()

    public enum SearchType {
        ID, TAG, INSTRUCTION, METHOD, ALL_LEAVES, ALL_SOURCES, OUT_OPEN, OUT_CLOSED, IN_OPEN, IN_CLOSED, ROOT_PATH
    }

    public MainTabController(File file, Graph graph) throws IOException {
        Controllers.loadFXML("/MainTabContent.fxml", this);
        this.vizPanelController = new VizPanelController(file, graph);
        this.centerPane.setCenter(this.vizPanelController.root);
        this.vizPanelController.initFX(graph);
        this.tab = new Tab(file.getName(), this.root);
        this.tab.tooltipProperty().set(new Tooltip(file.getAbsolutePath()));
        Controllers.put(this.tab, this);
    }

    public void repaintAll() {
        System.out.println("Repainting all...");
        bytecodeArea.setDescription();
        setRightText();
        searchResults.writeText(this.vizPanelController);
    }

    public void setRightText() {
        StringBuilder text = new StringBuilder();
        for (AbstractLayoutVertex v : this.vizPanelController.getHighlighted()) {
            text.append(v.getRightPanelContent() + "\n");
        }

        this.descriptionArea.setText(text.toString());
    }

    // Clean up info from previous searches
    public void initSearch(SearchType search) {
        this.vizPanelController.resetHighlighted(null);
        String query = getSearchInput(search);

        if (search == SearchType.ID) {
            searchByID(query); // TODO: Fix inconsistency with panel root
        } else if (search == SearchType.INSTRUCTION) {
            this.vizPanelController.getPanelRoot().searchByInstruction(query, vizPanelController);
        } else if (search == SearchType.METHOD) {
            this.vizPanelController.getPanelRoot().searchByMethod(query, vizPanelController);
        }

        this.repaintAll();
        /*Parameters.bytecodeArea.clear();
        Parameters.rightArea.setText("");*/
    }

    public String getSearchInput(SearchType search) {
        String title = "";
        System.out.println("Search type: " + search);
        if (search == SearchType.ID || search == SearchType.ROOT_PATH) {
            title = "Enter node ID(s)";
        } else if (search == SearchType.INSTRUCTION) {
            title = "Instruction contains ...";
        } else if (search == SearchType.METHOD) {
            title = "Method name contains ...";
        } else if (search == SearchType.OUT_OPEN || search == SearchType.OUT_CLOSED || search == SearchType.IN_OPEN
                || search == SearchType.IN_CLOSED) {
            title = "Enter node ID";
        }

        String input = "";
        if (search != SearchType.ALL_LEAVES && search != SearchType.ALL_SOURCES && search != SearchType.TAG) {
            System.out.println("Showing dialog...");
            TextInputDialog dialog = new TextInputDialog();
            dialog.setHeaderText(title);
            dialog.showAndWait();
            input = dialog.getResult();
            if (input == null) {
                return "";
            } else {
                input = input.trim();
            }

            if (input.equals("")) {
                return "";
            }
        }

        return input;
    }

    public void searchByID(String input) {
        for (String token : input.split(", ")) {
            if (token.trim().equalsIgnoreCase("")) {
                /* Do nothing */
            } else if (token.indexOf('-') == -1) {
                int id1 = Integer.parseInt(token.trim());
                this.vizPanelController.getPanelRoot().searchByID(id1, vizPanelController);
            } else {
                int id1 = Integer.parseInt(token.substring(0, token.indexOf('-')).trim());
                int id2 = Integer.parseInt(token.substring(token.lastIndexOf('-') + 1).trim());
                this.vizPanelController.getPanelRoot().searchByIDRange(id1, id2, vizPanelController);
            }
        }
    }
}