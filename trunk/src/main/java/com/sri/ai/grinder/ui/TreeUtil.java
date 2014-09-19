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
package com.sri.ai.grinder.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.WindowConstants;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.DefaultParsingProcess;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.expresso.api.Expression;

/**
 * 
 * @author braz
 *
 */
@Beta
public class TreeUtil {
	private static final boolean keepExpandedNodesOpen = true; // set to true,
																// if you want
																// the tree
																// reopen the
																// expanded
																// nodes after a
																// new node is
																// added.
	//
	protected static ExpressionNode activeJustificationNode,
			rootJustificationNode = new ExpressionNode("", null);
	protected static DefaultTreeModel treeJustificationModel;
	protected static ExpressionTreeView jJustificationTree;
	//
	protected static ExpressionNode activeTraceNode,
			rootTraceNode = new ExpressionNode("", null);
	protected static DefaultTreeModel treeTraceModel;
	protected static ExpressionTreeView jTraceTree;
	//
	protected static JFrame frame;
	protected static Writer writer;
	protected static final String CLOSE = "CLOSE";
	protected static final String EXIT = "EXIT";
	
	// Note: Just for standalone testing.
	public static void main(String[] args) {
		TreeUtil.setWriter(DefaultWriter.newDefaultConfiguredWriter());
		TreeUtil.displayExpressionsTrees();
		
		TreeUtil.addTrace("root");
		TreeUtil.startTraceLevel();
		
		TreeUtil.addTrace("root:1");
		TreeUtil.startTraceLevel();
		TreeUtil.addTrace("root:1:1");
		TreeUtil.addTrace("root:1:2");
		TreeUtil.endTraceLevel();
		
		TreeUtil.addTrace("root:2");
		TreeUtil.startTraceLevel();
		TreeUtil.addTrace("root:2:1");
		TreeUtil.startTraceLevel();
		TreeUtil.addTrace("root:2:1:1");
		TreeUtil.addTrace("root:2:1:2");
		TreeUtil.endTraceLevel();
		TreeUtil.addTrace("root:2:2");
		TreeUtil.endTraceLevel();
		
		TreeUtil.endTraceLevel();
	}

	public static void setWriter(Writer writer) {
		TreeUtil.writer = writer;
	}

	public static Writer getWriter() {
		return writer;
	}

	public static Expression parse(String expressionString, Grammar grammer) {
		ParsingProcess process = new DefaultParsingProcess(expressionString,
				grammer);
		Expression result = process.parseOfNonTerminal("Expression");
		return result;
	}

	public static boolean isShowing() {
		if (frame == null) {
			return false;
		}
		return frame.isVisible();
	}
	
	public static void flushData() {
		if (frame != null) {
			rootJustificationNode = new ExpressionNode("", null);
			activeJustificationNode = rootJustificationNode;
			treeJustificationModel = new DefaultTreeModel(rootJustificationNode);
			jJustificationTree.setModel(treeJustificationModel);
			
			rootTraceNode = new ExpressionNode("", null);
			activeTraceNode = rootTraceNode;
			treeTraceModel = new DefaultTreeModel(rootTraceNode);
			jTraceTree.setModel(treeTraceModel);
		}
	}

	public static void displayExpressionsTrees() {
		if (frame != null) {
			frame.setVisible(true);
			return;
		}

		// 1. Create the frame.
		frame = new JFrame("Expression Trees");

		// 2. Optional: What happens when the frame closes?
		frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE); // .EXIT_ON_CLOSE);
		frame.setPreferredSize(new Dimension(800, 800));
		frame.getContentPane().add(createToolbar(), BorderLayout.NORTH);

		// Create the justification view
		treeJustificationModel = new DefaultTreeModel(rootJustificationNode);
		activeJustificationNode = rootJustificationNode;
		jJustificationTree = new ExpressionTreeView(treeJustificationModel,
				keepExpandedNodesOpen);
		jJustificationTree.setEditable(true);
		jJustificationTree.setRootVisible(false);
		jJustificationTree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		jJustificationTree.setShowsRootHandles(true);

		JScrollPane spJustification = new JScrollPane();
		spJustification.getViewport().add(jJustificationTree);

		// Create the trace view
		treeTraceModel = new DefaultTreeModel(rootTraceNode);
		activeTraceNode = rootTraceNode;
		jTraceTree = new ExpressionTreeView(treeTraceModel,
				keepExpandedNodesOpen);
		jTraceTree.setEditable(true);
		jTraceTree.setRootVisible(false);
		jTraceTree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		jTraceTree.setShowsRootHandles(true);

		JScrollPane spTrace = new JScrollPane();
		spTrace.getViewport().add(jTraceTree);
		
		JPanel justificationPanel = new JPanel();
		justificationPanel.setLayout(new BorderLayout(0, 0));
		justificationPanel.add(spJustification, BorderLayout.CENTER);
		justificationPanel.add(jJustificationTree.getFindPanel(), BorderLayout.SOUTH);
		
		JPanel tracePanel = new JPanel();
		tracePanel.setLayout(new BorderLayout(0, 0));
		tracePanel.add(spTrace, BorderLayout.CENTER);
		tracePanel.add(jTraceTree.getFindPanel(), BorderLayout.SOUTH);

		// Place both the justification and trace views into a tabbed pane
		JTabbedPane tabbedPane = new JTabbedPane();
		tabbedPane.addTab("Justification", justificationPanel);
		tabbedPane.addTab("Trace", tracePanel);

		frame.getContentPane().add(tabbedPane, BorderLayout.CENTER);
		frame.setSize(800, 800);
		frame.setVisible(true);
	}

	private static JToolBar createToolbar() {
		JToolBar toolbar = new JToolBar();
		toolbar.setFloatable(false);
		JButton button = null;

		button = makeNavigationButton(CLOSE, "Close the expression window",
				"Close Window");
		toolbar.add(button);
		button = makeNavigationButton(EXIT, "Exit the program", "Exit Program");
		toolbar.add(button);
		return toolbar;
	}

	protected static JButton makeNavigationButton(String actionCommand,
			String toolTipText, String altText) {

		// Create and initialize the button.
		JButton button = new JButton();
		button.setActionCommand(actionCommand);
		button.setToolTipText(toolTipText);
		button.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				String actionCommand = e.getActionCommand();

				if (actionCommand.equals(CLOSE)) {
					disposeExpressionsTree();
				}

				if (actionCommand.equals(EXIT)) {
					System.exit(0);
				}
			}

		});

		button.setText(altText);
		return button;
	}

	public static void disposeExpressionsTree() {
		frame.dispose();
	}

	//
	// START - Justification output routines
	public static void startJustificationLevel() {
		if (activeJustificationNode.getChildCount() == 0) {
			// Add an indenter for levels that had no output.
			addJustification(">>");
		} 
		
		activeJustificationNode = (ExpressionNode) activeJustificationNode
				.getChildAt(activeJustificationNode.getChildCount() - 1);
	}

	public static void endJustificationLevel() {
		activeJustificationNode = (ExpressionNode) activeJustificationNode
				.getParent();
		if (activeJustificationNode == null) {
			activeJustificationNode = rootJustificationNode;
		}
	}

	public static void addJustification(Object obj) {
		activeJustificationNode.add(obj);
		EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				treeJustificationModel.reload();
				jJustificationTree.restoreExpandedPaths();
			}
		});
	}

	// END - Justification output routines
	//

	//
	// START - Trace ouput routines
	public static void startTraceLevel() {
		if (activeTraceNode.getChildCount() == 0) {
			// Add an indenter for levels that had no output.
			addTrace(">>");
		} 

		activeTraceNode = (ExpressionNode) activeTraceNode
					.getChildAt(activeTraceNode.getChildCount() - 1);
	}

	public static void endTraceLevel() {
		activeTraceNode = (ExpressionNode) activeTraceNode.getParent();
		if (activeTraceNode == null) {
			activeTraceNode = rootTraceNode; 
		}
	}

	public static void addTrace(Object obj) {
		activeTraceNode.add(obj);
		EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				treeTraceModel.reload();
				jTraceTree.restoreExpandedPaths();
			}
		});
	}

	public static void addExpressionTrace(String expr) {
		Expression expression = parse(expr, writer.getGrammar());
		addTrace(expression);
	}

	// END - Trace output routines
	//

	public static void waitUntilUIClosed() {
		while (isShowing()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException ex) {
			}
		}
	}
}
