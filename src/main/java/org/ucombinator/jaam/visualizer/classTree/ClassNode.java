package org.ucombinator.jaam.visualizer.classTree;

import javafx.scene.control.CheckBoxTreeItem;
import javafx.scene.control.TreeItem;
import javafx.scene.paint.Color;
import org.controlsfx.glyphfont.FontAwesome;
import org.ucombinator.jaam.visualizer.controllers.CodeViewController;
import org.ucombinator.jaam.visualizer.layout.CodeEntity;
import org.ucombinator.jaam.visualizer.state.StateVertex;
import org.ucombinator.jaam.visualizer.main.Main;

import java.util.HashMap;
import java.util.HashSet;

public class ClassNode extends ClassTreeNode {

    HashMap<String, MethodNode> methods;
    HashSet<FieldNode> fields;

    ClassNode(String name, String prefix) {
        super(name,prefix);

        methods = new HashMap<>();
        fields = new HashSet<>();
    }

    @Override
    public String toString() {
        return super.toString();
    }

    @Override
    public HashSet<StateVertex> getChildVertices() {

        HashSet<StateVertex> all = new HashSet<>();
        methods.values().stream().forEach(m -> all.addAll(m.getChildVertices()));

        return all;
    }

    @Override
    public void handleDoubleClick(CodeViewController codeView) {
        codeView.displayCodeTab(this.name);
    }

    public boolean addVertex(StateVertex vertex) {

        assert vertex instanceof CodeEntity;
        assert ((CodeEntity) vertex).getClassName().startsWith(this.getName()); //Maybe should be fullCompare...

        CodeEntity c = (CodeEntity)vertex;

        MethodNode node = methods.get(c.getMethodName());

        if (node == null) {
            methods.put(c.getMethodName(), new MethodNode(vertex));
        }
        else {
            node.addVertex(vertex);
        }

        return true;
    }

    public void addFields(CodeViewController codeViewController) {

        codeViewController.getFields(this.getName()).forEach( f -> System.out.println());

        codeViewController.getFields(this.getName()).forEach(
                fieldName -> fields.add(new FieldNode(fieldName, this.getName()) )
        );

    }

    public void build(TreeItem<ClassTreeNode> parent) {
        CheckBoxTreeItem<ClassTreeNode> item = buildTreeItem(parent);

        item.setGraphic(Main.getIconFont().create(FontAwesome.Glyph.COPYRIGHT).color(Color.ORANGE));

        methods.values().stream().sorted().forEach(m -> m.build(item));
        fields.stream().sorted().forEach(f -> f.build(item));
    }


}
