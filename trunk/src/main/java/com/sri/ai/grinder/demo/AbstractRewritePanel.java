/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.demo;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.JSplitPane;
import java.awt.Dimension;
import javax.swing.JLabel;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import java.awt.FlowLayout;
import javax.swing.JTabbedPane;
import javax.swing.border.TitledBorder;
import javax.swing.BoxLayout;
import javax.swing.ComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JScrollPane;
import javax.swing.JEditorPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellEditor;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.JTextArea;

import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.demo.model.EnableItem;
import com.sri.ai.grinder.demo.model.ExampleRewrite;
import com.sri.ai.grinder.demo.model.GroupEnableItem;
import com.sri.ai.grinder.demo.model.LeafEnableItem;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.NestedArithmeticOperation;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.number.UnaryMinus;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AbstractRewritePanel extends JPanel {

	private static final long serialVersionUID = 1L;
	
	//
	private DefaultMutableTreeNode exampleRewritersRootNode = null;
	//
	private JEditorPane inputExpressionEditor;
	private JComboBox exampleComboBox;
	private JTree rewriterEnableTree;

	/**
	 * Create the panel.
	 */
	public AbstractRewritePanel() {
		setLayout(new BorderLayout(0, 0));
		
		JSplitPane mainSplitPane = new JSplitPane();
		mainSplitPane.setOneTouchExpandable(true);
		mainSplitPane.setResizeWeight(1.0);
		add(mainSplitPane, BorderLayout.CENTER);
		
		JSplitPane expressionAndOutputSplitPane = new JSplitPane();
		expressionAndOutputSplitPane.setPreferredSize(new Dimension(380, 500));
		expressionAndOutputSplitPane.setOneTouchExpandable(true);
		expressionAndOutputSplitPane.setResizeWeight(1.0);
		expressionAndOutputSplitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		mainSplitPane.setLeftComponent(expressionAndOutputSplitPane);
		
		JPanel expressionPanel = new JPanel();
		expressionPanel.setPreferredSize(new Dimension(400, 300));
		expressionAndOutputSplitPane.setLeftComponent(expressionPanel);
		expressionPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel examplePanel = new JPanel();
		FlowLayout flowLayout = (FlowLayout) examplePanel.getLayout();
		flowLayout.setAlignment(FlowLayout.LEFT);
		expressionPanel.add(examplePanel, BorderLayout.NORTH);
		
		JLabel exampleLabel = new JLabel("Example:");
		examplePanel.add(exampleLabel);
		
		exampleComboBox = new JComboBox();
		exampleComboBox.setPreferredSize(new Dimension(240, 25));
		exampleComboBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				ExampleRewrite eg = (ExampleRewrite) exampleComboBox.getItemAt(exampleComboBox.getSelectedIndex());
				if (eg != null) {
					inputExpressionEditor.setText(eg.getInputExpression());
				}
			}
		});
		exampleComboBox.setModel(getExampleComboBoxModel());
		examplePanel.add(exampleComboBox);
		
		JButton button = new JButton("| >");
		button.setToolTipText("Single rewrte step.");
		examplePanel.add(button);
		
		JButton button_1 = new JButton("->");
		button_1.setToolTipText("Exhaustve Rewrite");
		examplePanel.add(button_1);
		
		JButton button_2 = new JButton("Clear");
		button_2.setToolTipText("Clear All");
		examplePanel.add(button_2);
		
		JPanel expressionViews = new JPanel();
		expressionPanel.add(expressionViews, BorderLayout.CENTER);
		expressionViews.setLayout(new BoxLayout(expressionViews, BoxLayout.Y_AXIS));
		
		JPanel expressionInputPanel = new JPanel();
		expressionInputPanel.setBorder(new TitledBorder(null, "Input", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		expressionViews.add(expressionInputPanel);
		expressionInputPanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane inputExpressionEditorScrollPane = new JScrollPane();
		expressionInputPanel.add(inputExpressionEditorScrollPane, BorderLayout.CENTER);
		
		inputExpressionEditor = new JEditorPane();
		inputExpressionEditor.setText("2+2");
		inputExpressionEditorScrollPane.setViewportView(inputExpressionEditor);
		
		JPanel expressionOutputPanel = new JPanel();
		expressionOutputPanel.setBorder(new TitledBorder(null, "Output", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		expressionViews.add(expressionOutputPanel);
		expressionOutputPanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane outputExpressionEditorScrollPane = new JScrollPane();
		expressionOutputPanel.add(outputExpressionEditorScrollPane, BorderLayout.CENTER);
		
		JEditorPane outputExpressionEditor = new JEditorPane();
		outputExpressionEditor.setText("4");
		outputExpressionEditorScrollPane.setViewportView(outputExpressionEditor);
		
		JPanel outputPanel = new JPanel();
		outputPanel.setPreferredSize(new Dimension(300, 160));
		expressionAndOutputSplitPane.setRightComponent(outputPanel);
		outputPanel.setLayout(new BorderLayout(0, 0));
		
		JTabbedPane outputPane = new JTabbedPane(JTabbedPane.TOP);
		outputPanel.add(outputPane, BorderLayout.CENTER);
		
		JPanel consolePanel = new JPanel();
		outputPane.addTab("Console", null, consolePanel, null);
		consolePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane consoleScrollPane = new JScrollPane();
		consolePanel.add(consoleScrollPane, BorderLayout.CENTER);
		
		JTextArea txtrTraceOutput = new JTextArea();
		txtrTraceOutput.setText("| Trace Output");
		consoleScrollPane.setViewportView(txtrTraceOutput);
		
		JPanel tracePanel = new JPanel();
		outputPane.addTab("Trace", null, tracePanel, null);
		
		JPanel controlPanel = new JPanel();
		controlPanel.setPreferredSize(new Dimension(300, 500));
		mainSplitPane.setRightComponent(controlPanel);
		controlPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel rewriterSelectionPanel = new JPanel();
		rewriterSelectionPanel.setBorder(new TitledBorder(null, "Rewriters", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		controlPanel.add(rewriterSelectionPanel, BorderLayout.CENTER);
		rewriterSelectionPanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane activeRewritersScrollPane = new JScrollPane();
		rewriterSelectionPanel.add(activeRewritersScrollPane, BorderLayout.CENTER);
		
		rewriterEnableTree = new JTree();
		rewriterEnableTree.setRootVisible(false);
		rewriterEnableTree.setModel(getRewriterEnabledTreeModel());
		activeRewritersScrollPane.setViewportView(rewriterEnableTree);

		postGUIInitialization();
	}
	
	//
	// PROTECTED
	//
	
	/**
	 * To be overridden by sub-classes.
	 * @return the example rewrite expressions.
	 */
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			new ExampleRewrite("1. Addition 1 + 1", "1 + 1"),
			new ExampleRewrite("2. Addition 2 + 2", "2 + 2"),
			new ExampleRewrite("3. Addition 3 + 3", "3 + 3")
		};
	}
	
	protected EnableItem<Rewriter> getExampleRewriters() {
		
		List<EnableItem<Rewriter>> basicRewriters = new ArrayList<EnableItem<Rewriter>>();
		basicRewriters.add(new LeafEnableItem<Rewriter>("Plus",  new Plus()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Minus", new Minus()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Unary Minus", new UnaryMinus()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Times", new Times()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Division", new Division()));
		GroupEnableItem<Rewriter> basicGroup = new GroupEnableItem<Rewriter>("Basic", basicRewriters);
				
		List<EnableItem<Rewriter>> advancedRewriters = new ArrayList<EnableItem<Rewriter>>();
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Exponentiation",  new Exponentiation()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Nested Arithmetic Operation", new NestedArithmeticOperation()));
		GroupEnableItem<Rewriter> advancedGroup = new GroupEnableItem<Rewriter>("Advanced", advancedRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(basicGroup);
		groups.add(advancedGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Addition Rewriters", groups);
				
		return root; 
	}
	
	//
	// PRIVATE
	//
	private void postGUIInitialization() {
		exampleComboBox.setSelectedIndex(0);
		DefaultTreeCellRenderer renderer = new RewriterEnableTreeRenderer();
		rewriterEnableTree.setCellRenderer(renderer);
		rewriterEnableTree.setCellEditor(new RewriterEnableTreeCellEditor(rewriterEnableTree, renderer));
		rewriterEnableTree.setEditable(true);
		for (int i = 0; i < rewriterEnableTree.getRowCount(); i++) {
			rewriterEnableTree.expandRow(i);
		}
	}
	
	private ComboBoxModel getExampleComboBoxModel() {
		return new DefaultComboBoxModel(getExampleRewrites());
	}
	
	private TreeModel getRewriterEnabledTreeModel() {
		EnableItem<Rewriter> rootEnableItem = getExampleRewriters();

		exampleRewritersRootNode = new DefaultMutableTreeNode(exampleRewritersRootNode);
		populateChildrenNodes(exampleRewritersRootNode, rootEnableItem.getChildren());
		
		return new DefaultTreeModel(exampleRewritersRootNode);
	}
	
	private void populateChildrenNodes(DefaultMutableTreeNode parentNode, List<EnableItem<Rewriter>> children) {
		for (EnableItem<Rewriter> child : children) {
			DefaultMutableTreeNode childNode = new DefaultMutableTreeNode(child);
			parentNode.add(childNode);
			populateChildrenNodes(childNode, child.getChildren());
		}
	}
	
	private class RewriterEnableTreeRenderer extends DefaultTreeCellRenderer {
		private static final long serialVersionUID = 1L;
		private Map<EnableItem<Rewriter>, JCheckBox> checkBoxes = new HashMap<EnableItem<Rewriter>, JCheckBox>();

		public RewriterEnableTreeRenderer() {
		}

		public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
			super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
			
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
			@SuppressWarnings("unchecked")
			final EnableItem<Rewriter> enableItem = (EnableItem<Rewriter>) node.getUserObject();
			
			JCheckBox checkBox = findCheckBox(enableItem);	
			
			Component result = checkBox;
			if (null == checkBox) {
				result = this;
			}
			
			return result;
		}
		
		private JCheckBox findCheckBox(final EnableItem<Rewriter> item) {
			JCheckBox checkBox = checkBoxes.get(item);
			if (checkBox == null && item.getChildren().size() == 0) {
				checkBox = new JCheckBox(item.toString());
				checkBox.setSelected(item.isEnabled());
				checkBox.setFocusable(true);
				checkBox.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent actionEvent) {
						JCheckBox checkBox = (JCheckBox) actionEvent.getSource();						
						item.setEnabled(checkBox.isSelected());
					}
				});	
				
				checkBoxes.put(item, checkBox);
			}
			
			return checkBox;
		}
	}
	
	private class RewriterEnableTreeCellEditor extends DefaultTreeCellEditor {
		private DefaultTreeCellRenderer renderer = null;
		
		public RewriterEnableTreeCellEditor(JTree tree, DefaultTreeCellRenderer renderer) {
			super(tree, renderer);
			this.renderer = renderer;
		}
		
		@Override
		public boolean isCellEditable(EventObject event) {
			super.isCellEditable(event);

			boolean editable = false;

			if (event instanceof MouseEvent) {

				MouseEvent mouseEvent = (MouseEvent) event;
				TreePath path = tree.getPathForLocation(mouseEvent.getX(),
						mouseEvent.getY());

				if (path != null) {

					Object node = path.getLastPathComponent();
					if ((node != null)
							&& (node instanceof DefaultMutableTreeNode)) {
						DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) node;
						if (treeNode.isLeaf()) {
							editable = true;
						}
					}
				}
			}
			return editable;
		}
		
		public Component getTreeCellEditorComponent(JTree tree,
                Object value,
                boolean sel,
                boolean expanded,
                boolean leaf,
                int row) {		
			Component component = renderer.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, true);
			if (component == null) {
				component = super.getTreeCellEditorComponent(tree, value, sel, expanded, leaf, row);
			}
			
			return component;
		}
	}
}
