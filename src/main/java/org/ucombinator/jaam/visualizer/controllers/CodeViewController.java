package org.ucombinator.jaam.visualizer.controllers;

import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.scene.layout.BorderPane;
import org.ucombinator.jaam.visualizer.codeView.CodeAreaGenerator;
import org.ucombinator.jaam.visualizer.gui.SelectEvent;
import org.ucombinator.jaam.visualizer.layout.*;
import java.io.IOException;

public class CodeViewController {

    @FXML public final VBox root = null; // Initialized by Controllers.loadFXML()
    @FXML public final TabPane codeTabs = null; // Initialized by Controllers.loadFXML()

    CodeAreaGenerator codeAreaGenerator;

    public CodeViewController(CodeAreaGenerator codeAreaGenerator) throws IOException {
        Controllers.loadFXML("/CodeView.fxml", this);

        this.codeAreaGenerator = codeAreaGenerator;

    }

    public void addSelectHandler(BorderPane centerPane) {
        centerPane.addEventHandler(SelectEvent.VERTEX_SELECTED, onVertexSelect);
    }

    EventHandler<SelectEvent> onVertexSelect = new EventHandler<SelectEvent>() {
        @Override
        public void handle(SelectEvent selectEvent) {

            AbstractLayoutVertex av = selectEvent.getVertex();

            if(av instanceof CodeEntity)
            {
                CodeEntity v = (CodeEntity)av;

                Tab t = codeTabs.getTabs().stream().filter(c-> c.getId().equals(v.getClassName())).findFirst().orElse(null);

                if(t == null)
                {
                    t= new Tab(v.getShortClassName(), codeAreaGenerator.generateCodeArea(v.getClassName()) );
                    t.setId(v.getClassName());
                    codeTabs.getTabs().add(t);
                }

                codeTabs.getSelectionModel().select(t);
            }

            /*
            if(av instanceof LayoutMethodVertex)
            {
                LayoutMethodVertex v = (LayoutMethodVertex)av;

                Tab newTab = new Tab(v.getShortClassName(), codeAreaGenerator.generateCodeArea(v.getClassName()) );
                codeTabs.getTabs().add(newTab);
            }
            if(av instanceof LayoutLoopVertex)
            {
                LayoutLoopVertex v = (LayoutLoopVertex) av;

                Tab newTab = new Tab(v.getShortClassName(), codeAreaGenerator.generateCodeArea(v.getClassName()) );

                newTab.setTooltip(new Tooltip(v.getClassName()));

                codeTabs.getTabs().add(newTab);
            }
            */
        }
    };


}