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
import java.awt.EventQueue;
import java.awt.Font;

import javax.swing.JSplitPane;
import java.awt.Dimension;
import javax.swing.JLabel;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JTabbedPane;
import javax.swing.border.TitledBorder;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.BoxLayout;
import javax.swing.ComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellEditor;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import javax.swing.JTextArea;

import org.slf4j.ILoggerFactory;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.Parser;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.OpenInterpretationModule;
import com.sri.ai.grinder.core.RewriteOnce;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.demo.model.EnableItem;
import com.sri.ai.grinder.demo.model.ExampleRewrite;
import com.sri.ai.grinder.demo.model.GroupEnableItem;
import com.sri.ai.grinder.demo.model.LeafEnableItem;
import com.sri.ai.grinder.demo.model.Options;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.RewriterLoggingNamedRewriterFilter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.AbsorbingElement;
import com.sri.ai.grinder.library.Associative;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.Distributive;
import com.sri.ai.grinder.library.PlainSubstitution;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.SyntacticFunctionsSubExpressionsProvider;
import com.sri.ai.grinder.library.boole.ForAllSubExpressionsAndScopedVariablesProvider;
import com.sri.ai.grinder.library.boole.ThereExistsSubExpressionsAndScopedVariablesProvider;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.controlflow.ImposedConditionsModule;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierElimination;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierEliminationWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplify;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSetSubExpressionsProvider;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSetSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.grinder.ui.ExpressionNode;
import com.sri.ai.grinder.ui.ExpressionTreeView;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.util.log.LogX;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Beta
public class AbstractRewritePanel extends JPanel {

	private static final long serialVersionUID = 1L;
	private static final String _iconResolution = "22x22";
	
	//
	private Options options = null;
	//
	private UndoManager undoManager = new UndoManager();
	private ExpressionNode activeTraceNode, rootTraceNode = new ExpressionNode("", null);
	private DefaultTreeModel treeTraceModel = new DefaultTreeModel(rootTraceNode);
	//
	//
	private LoggerContext               loggerContext = null;
	private AppenderBase<ILoggingEvent> traceAppender = null;
	//
	private DefaultMutableTreeNode exampleRewritersRootNode = null;
	private List<EnableItem<Rewriter>> rewriterEnableList   = new ArrayList<EnableItem<Rewriter>>();
	private String lastSingleStepInput = "";
	// 
	private ImageIcon imageStep        = createImageIcon("media-skip-forward"+_iconResolution+".png");
	private ImageIcon imageExhaustive  = createImageIcon("media-seek-forward"+_iconResolution+".png");
	private ImageIcon imageUndo        = createImageIcon("edit-undo"+_iconResolution+".png");
	private ImageIcon imageRedo        = createImageIcon("edit-redo"+_iconResolution+".png");
	private ImageIcon imageClear       = createImageIcon("edit-clear"+_iconResolution+".png");
	//
	private ExpressionEditor inputContextExpressionEditor;
	private ExpressionEditor inputExpressionEditor;
	private ExpressionEditor outputContextExpressionEditor;
	private ExpressionEditor outputExpressionEditor;
	private JComboBox exampleComboBox;
	private JTree rewriterEnableTree;
	private JTextArea consoleOutputTextArea;
	private ExpressionTreeView traceTree;
	private JPanel optionsPanel;
	private JButton btnUndo;
	private JButton btnRedo;

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
		expressionAndOutputSplitPane.setResizeWeight(0.3);
		expressionAndOutputSplitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		mainSplitPane.setLeftComponent(expressionAndOutputSplitPane);
		
		JPanel expressionPanel = new JPanel();
		expressionPanel.setPreferredSize(new Dimension(450, 360));
		expressionAndOutputSplitPane.setLeftComponent(expressionPanel);
		expressionPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel examplePanel = new JPanel();
		expressionPanel.add(examplePanel, BorderLayout.NORTH);
		examplePanel.setLayout(new BorderLayout(0, 0));
		
		JPanel panel = new JPanel();
		examplePanel.add(panel, BorderLayout.WEST);
		panel.setLayout(new BorderLayout(0, 0));
		
		optionsPanel = new JPanel();
		panel.add(optionsPanel, BorderLayout.CENTER);
		optionsPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel deaultsPanel = new JPanel();
		panel.add(deaultsPanel, BorderLayout.WEST);
		deaultsPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel actionButtonsPanel = new JPanel();
		deaultsPanel.add(actionButtonsPanel, BorderLayout.SOUTH);
		
		JButton btnRewriteSingle = new JButton("| >");
		actionButtonsPanel.add(btnRewriteSingle);
		btnRewriteSingle.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				String currentSingleStepInput = inputExpressionEditor.getText();
				if (currentSingleStepInput.equals(lastSingleStepInput)) {
					currentSingleStepInput = outputExpressionEditor.getText();
					inputExpressionEditor.setText(currentSingleStepInput);
				}
				System.out.println("------------");
				performRewrite(false);
				System.out.println("");
				lastSingleStepInput = currentSingleStepInput;
			}
		});
		btnRewriteSingle.setToolTipText("Single rewrite step");
		btnRewriteSingle.setIcon(imageStep);
		btnRewriteSingle.setText("");
		
		JButton btnRewriteExhaustive = new JButton("->");
		actionButtonsPanel.add(btnRewriteExhaustive);
		btnRewriteExhaustive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.out.println("------------");
				performRewrite(true);
				System.out.println("");
			}
		});
		btnRewriteExhaustive.setToolTipText("Exhaustive Rewrite");
		btnRewriteExhaustive.setIcon(imageExhaustive);
		btnRewriteExhaustive.setText("");
		
		btnUndo = new JButton("");
		actionButtonsPanel.add(btnUndo);
		btnUndo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (undoManager.canUndo()) {
					try {
						undoManager.undo();						
					} catch (CannotUndoException cue) {
						// ignore
					}
				}
				handleUndoRedo();
			}
		});
		btnUndo.setToolTipText("Undo Input Expression");
		btnUndo.setIcon(imageUndo);
		btnUndo.setText("");
		
		btnRedo = new JButton("");
		actionButtonsPanel.add(btnRedo);
		btnRedo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (undoManager.canRedo()) {
					try {
						undoManager.redo();						
					} catch (CannotRedoException cue) {
						// ignore
					}
				}
				handleUndoRedo();
			}
		});
		btnRedo.setToolTipText("Redo Input Expression");
		btnRedo.setIcon(imageRedo);
		btnRedo.setText("");
		
		JButton btnClear = new JButton("Clear");
		actionButtonsPanel.add(btnClear);
		btnClear.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				consoleOutputTextArea.setText("");
				clearTraceTree();
			}
		});
		btnClear.setToolTipText("Clear Console and Trace");
		btnClear.setIcon(imageClear);
		btnClear.setText("");
		
		JPanel exampleSelectionPanel = new JPanel();
		deaultsPanel.add(exampleSelectionPanel, BorderLayout.NORTH);
		
		JLabel exampleLabel = new JLabel("Example:");
		exampleSelectionPanel.add(exampleLabel);
		
		exampleComboBox = new JComboBox();
		exampleSelectionPanel.add(exampleComboBox);
		exampleComboBox.setPreferredSize(new Dimension(214, 25));
		exampleComboBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				ExampleRewrite eg = (ExampleRewrite) exampleComboBox.getItemAt(exampleComboBox.getSelectedIndex());
				if (eg != null) {
					inputContextExpressionEditor.setText(eg.getInputContext());
					inputExpressionEditor.setText(eg.getInputExpression());
				}
			}
		});
		exampleComboBox.setModel(getExampleComboBoxModel());
		
		
		
		JPanel expressionViews = new JPanel();
		expressionPanel.add(expressionViews, BorderLayout.CENTER);
		expressionViews.setLayout(new BoxLayout(expressionViews, BoxLayout.Y_AXIS));
		
		JPanel expressionInputPanel = new JPanel();
		expressionInputPanel.setBorder(new TitledBorder(null, "Input", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		expressionViews.add(expressionInputPanel);
		expressionInputPanel.setLayout(new BorderLayout(0, 0));
		
		
		
		inputExpressionEditor = new ExpressionEditor();
		expressionInputPanel.add(inputExpressionEditor, BorderLayout.CENTER);
		
		JPanel inputContextPanel = new JPanel();
		expressionInputPanel.add(inputContextPanel, BorderLayout.NORTH);
		inputContextPanel.setLayout(new BorderLayout(0, 0));
		
		JLabel lblNewLabel = new JLabel("Context:");
		inputContextPanel.add(lblNewLabel, BorderLayout.WEST);
		
		inputContextExpressionEditor = new ExpressionEditor();
		inputContextPanel.add(inputContextExpressionEditor);
		
		JPanel expressionOutputPanel = new JPanel();
		expressionOutputPanel.setBorder(new TitledBorder(null, "Output", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		expressionViews.add(expressionOutputPanel);
		expressionOutputPanel.setLayout(new BorderLayout(0, 0));
		
		
		outputExpressionEditor = new ExpressionEditor();
		expressionOutputPanel.add(outputExpressionEditor, BorderLayout.CENTER);
		
		JPanel outputContextPanel = new JPanel();
		outputExpressionEditor.add(outputContextPanel, BorderLayout.NORTH);
		outputContextPanel.setLayout(new BorderLayout(0, 0));
		
		JLabel lblNewLabel_1 = new JLabel("Context:");
		outputContextPanel.add(lblNewLabel_1, BorderLayout.WEST);
		
		outputContextExpressionEditor = new ExpressionEditor();
		outputContextPanel.add(outputContextExpressionEditor, BorderLayout.CENTER);
		
		JPanel outputPanel = new JPanel();
		outputPanel.setPreferredSize(new Dimension(300, 60));
		expressionAndOutputSplitPane.setRightComponent(outputPanel);
		outputPanel.setLayout(new BorderLayout(0, 0));
		
		JTabbedPane outputPane = new JTabbedPane(JTabbedPane.TOP);
		outputPanel.add(outputPane, BorderLayout.CENTER);
		
		JPanel consolePanel = new JPanel();
		outputPane.addTab("Console", null, consolePanel, null);
		consolePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane consoleScrollPane = new JScrollPane();
		consolePanel.add(consoleScrollPane, BorderLayout.CENTER);
		
		consoleOutputTextArea = new JTextArea();
		consoleOutputTextArea.setEditable(false);
		consoleScrollPane.setViewportView(consoleOutputTextArea);
		
		JPanel tracePanel = new JPanel();
		outputPane.addTab("Trace", null, tracePanel, null);
		tracePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane traceTreeScrollPane = new JScrollPane();
		tracePanel.add(traceTreeScrollPane, BorderLayout.CENTER);
		
		traceTree = new ExpressionTreeView(treeTraceModel, true);
		traceTree.setEditable(true);
		traceTree.setRootVisible(false);
		traceTree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		traceTree.setShowsRootHandles(true);
		traceTreeScrollPane.setViewportView(traceTree);
		
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
	
	public Options getOptions() {
		return options;
	}
	
	public void setOptions(Options options) {
		this.options = options;
	}
	
	public JPanel getOptionsPanel() {
		return optionsPanel;
	}
	
	public void setOptionsPanel(JPanel panel) {
		optionsPanel.add(panel, BorderLayout.CENTER);
	}
	
	public void notifySelected() {
		if (loggerContext == null) {
			while (loggerContext == null) {
				ILoggerFactory lf = LoggerFactory.getILoggerFactory();
				if (lf instanceof LoggerContext) {
					loggerContext = (LoggerContext) lf;
				} 
				else {
					try {
						Thread.sleep(100);
					} catch (Exception ex) {
						
					}
				}
			}
			traceAppender = new AppenderBase<ILoggingEvent>() {
				//
				private int currentIndentLevel = 0;
				private boolean firstTime = true;
				//
				@Override
				protected void append(ILoggingEvent eventObject) {
					String msg = eventObject.getFormattedMessage();
					Object[] args = eventObject.getArgumentArray();

					int indentLevel = LogX.getTraceLevel(eventObject.getLoggerName());

					while (indentLevel > currentIndentLevel) {
						if (!firstTime) {
							addTrace(">>");
						}
						startTraceLevel();
						currentIndentLevel++;
					}
					
					firstTime = false;

					StringBuilder sb = new StringBuilder(msg);
					while (indentLevel < currentIndentLevel) {
						endTraceLevel();
						currentIndentLevel--;
					}
					
					// Suffix the profiler information to the message
					// if available.
					Long profileInfo = LogX.getProfileInfo(eventObject.getLoggerName());
					if (profileInfo != null) {
						sb.append(" [");
						// Convert nanoseconds to milliseconds
						sb.append(profileInfo / 1000000);
						sb.append("ms.]");
					}

					if (msg != null && !msg.equals("")) {
						addTrace(sb.toString());
					}

					if (args != null) {
						for (Object arg : args) {
							addTrace(arg);
						}
					}
				}
			};
			
			traceAppender.addFilter(new RewriterLoggingNamedRewriterFilter());
			traceAppender.setContext(loggerContext);
			loggerContext.getLogger(Trace.getDefaultLoggerName()).addAppender(traceAppender);
		}
		
		traceAppender.start();
	}
	
	public void notifyUnselected() {
		traceAppender.stop();
	}
	
	/**
	 * Provides a print stream which can be used to redirect standard output
	 * streams.
	 */
	public PrintStream getConsoleOutputPrintStream() {
		return new PrintStream(new ConsoleOutputStream());
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
		GroupEnableItem<Rewriter> basicGroup = new GroupEnableItem<Rewriter>("Basic", basicRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(basicGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Addition Rewriters", groups);
				
		return root; 
	}
	
	//
	// PRIVATE
	//
	private void postGUIInitialization() {
		//
		undoManager.setLimit(-1);
		//
		RewriteUndoableEditListener undoableEditListener = new RewriteUndoableEditListener();
		inputExpressionEditor.addUndoableEditListener(undoableEditListener);
		
		// Select the first e.g. by default
		exampleComboBox.setSelectedIndex(0);
		// Setup, populate and expand the rewriter selection tree
		DefaultTreeCellRenderer renderer = new RewriterEnableTreeRenderer();
		rewriterEnableTree.setCellRenderer(renderer);
		rewriterEnableTree.setCellEditor(new RewriterEnableTreeCellEditor(rewriterEnableTree, renderer));
		rewriterEnableTree.setEditable(true);
		for (int i = 0; i < rewriterEnableTree.getRowCount(); i++) {
			rewriterEnableTree.expandRow(i);
		}
		// Configure the output consoled window.
		consoleOutputTextArea.setFont(new Font(Font.MONOSPACED, consoleOutputTextArea.getFont().getStyle(), 14));
		
		inputContextExpressionEditor.setText("true");
		
		TreeUtil.setWriter(DefaultWriter.newDefaultConfiguredWriter());
		clearTraceTree();
	}
	
	private void handleUndoRedo() {
		btnUndo.setEnabled(undoManager.canUndo());
		btnRedo.setEnabled(undoManager.canRedo());
	}
	
	private void clearTraceTree() {
		rootTraceNode = new ExpressionNode("", null);
		activeTraceNode = rootTraceNode;
		treeTraceModel = new DefaultTreeModel(rootTraceNode);
		traceTree.setModel(treeTraceModel);
	}
	
	private void startTraceLevel() {
		if (activeTraceNode.getChildCount() == 0) {
			activeTraceNode = rootTraceNode;
		} 
		else {
			activeTraceNode = (ExpressionNode) activeTraceNode
					.getChildAt(activeTraceNode.getChildCount() - 1);
		}
	}
	
	private void endTraceLevel() {
		activeTraceNode = (ExpressionNode) activeTraceNode.getParent();
		if (activeTraceNode == null) {
			activeTraceNode = rootTraceNode; 
		}
	}

	private void addTrace(Object obj) {
		activeTraceNode.add(obj);
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				treeTraceModel.reload();
				traceTree.restoreExpandedPaths();
			}
		});
	}
	
	private void performRewrite(boolean exhaustive) {	
		Parser parser = new AntlrGrinderParserWrapper();
		
		Writer writer = DefaultWriter.newDefaultConfiguredWriter();
		
		Expression inputContext = parser.parse(inputContextExpressionEditor.getText());
		if (inputContext == null) {
			outputContextExpressionEditor.setText("ERROR: Malformed Input Context.");
		}
		Expression input = parser.parse(inputExpressionEditor.getText());
		if (input == null) {
			outputExpressionEditor.setText("ERROR: Malformed Input Expression.");
		}
		if (inputContext  != null && input != null) {
			List<Rewriter> rewriters = getRewritersAndModules();
			RewritingProcess process = new DefaultRewritingProcess(input, new RewriteOnce(rewriters));
			if (isCardinalityRewriterLookupNeeded(rewriters)) {
				((DefaultRewritingProcess)process).setRewriterLookup(new DefaultRewriterLookup(DirectCardinalityComputationFactory.getCardinalityRewritersMap()));
			}
			GrinderConfiguration.setProperty(GrinderConfiguration.KEY_ASSUME_DOMAIN_ALWAYS_LARGE, ""+getOptions().isAssumeDomainsAlwaysLarge());
			CardinalityTypeOfLogicalVariable.registerDomainSizeOfLogicalVariableWithProcess(new CardinalityTypeOfLogicalVariable.DomainSizeOfLogicalVariable() {
				@Override
				public Integer size(Expression logicalVariable, RewritingProcess process) {
					Integer result = null; // unknown by default
					if (getOptions().isDomainSizeKnown()) {
						result = getOptions().getDomainSize();
					}
					return result;
				}
			}, process);
			if (!Expressions.TRUE.equals(inputContext)) {
				process = GrinderUtil.extendContextualConstraint(inputContext, process);
			}
			
			Rewriter rewriter = null;
			if (exhaustive) {
				rewriter = new TotalRewriter(getRewritersAndModules());
			}
			else {
				rewriter = new RewriteOnce(getRewritersAndModules());
			}
			
			try {				
				Expression output = rewriter.rewrite(input, process);			
				try {
					outputContextExpressionEditor.setText(writer.toString(process.getContextualConstraint()));
					outputExpressionEditor.setText(writer.toString(output));					
				} catch (RuntimeException ire) {
					outputExpressionEditor.setText("// ERROR: Rewriting Output - \n"+output);
					ire.printStackTrace();
				}
			} catch (RuntimeException ore) {
				outputExpressionEditor.setText("// ERROR: Rewriting Input - \n"+inputExpressionEditor.getText());
				ore.printStackTrace();
			}
		}
	}
	
	private List<Rewriter> getRewritersAndModules() {
		List<Rewriter> rewriters = new ArrayList<Rewriter>();
		
		for (EnableItem<Rewriter> enabledRewriter : rewriterEnableList) {
			if (enabledRewriter.isEnabled()) {
				Rewriter r = enabledRewriter.getUserObject();
				rewriters.add(r);
				
			}
		}
		
		return rewriters;
	}
		
	private boolean isCardinalityRewriterLookupNeeded(List<Rewriter> rewriters) {
		boolean result = false;
		
		for (Rewriter r : rewriters) {
			if (r.getClass() == TopSimplify.class ||
					r.getClass() == QuantifierElimination.class ||
					r.getClass() == QuantifierEliminationWrapper.class ||
					r.getClass() == TopImpliedCertainty.class ||
					r.getClass() == CardinalityWrapper.class) {
				
				result = true;
				break;
			}
		}
		
		return result;
	}
		
	
	
	private ComboBoxModel getExampleComboBoxModel() {
		return new DefaultComboBoxModel(getExampleRewrites());
	}
	
	private TreeModel getRewriterEnabledTreeModel() {
		exampleRewritersRootNode = new DefaultMutableTreeNode(exampleRewritersRootNode);
		
		EnableItem<Rewriter> rootRewriterEnableItem = getExampleRewriters();
		populateChildrenNodes(exampleRewritersRootNode, rootRewriterEnableItem.getChildren());
		
		addModulesAndProviders();
		
		return new DefaultTreeModel(exampleRewritersRootNode);
	}
	
	private void populateChildrenNodes(DefaultMutableTreeNode parentNode, List<EnableItem<Rewriter>> children) {
		for (EnableItem<Rewriter> child : children) {
			DefaultMutableTreeNode childNode = new DefaultMutableTreeNode(child);
			parentNode.add(childNode);
			if (child.getChildren().size() > 0) {
				populateChildrenNodes(childNode, child.getChildren());
			}
			else {
				rewriterEnableList.add(child);
			}
		}
	}
	
	private void addModulesAndProviders() {
		// Important Expected Rewrite Behavior
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Plain Substitution",new PlainSubstitution()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Cardinality Type of Logical Variable", new CardinalityTypeOfLogicalVariable()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Absorbing Element", new AbsorbingElement(
				"and", "false",
				"or", "true",
				"*", "0")));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Associative",  new Associative("+", "*", "and")));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Distributive * +", new Distributive("*", "+")));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Distributive and or", new Distributive("and", "or")));
		
		// Modules
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Expression Knowledge", new ExpressionKnowledgeModule()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Imposed Conditions", new ImposedConditionsModule()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Scoped Variables", new ScopedVariables()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Open Interpretation Module", new OpenInterpretationModule()));
			
		// Providers
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("if . then . else - subexpression and imposed conditions provider", new IfThenElseSubExpressionsAndImposedConditionsProvider()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Intensional Set - subexpression and imposed conditions provider", new IntensionalSetSubExpressionsAndImposedConditionsProvider()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Extensional Set - subexpression provider", new ExtensionalSetSubExpressionsProvider()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("For All - subexpression and scoped variables provider", new ForAllSubExpressionsAndScopedVariablesProvider()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("There Exists - subexpression and scoped variables provider", new ThereExistsSubExpressionsAndScopedVariablesProvider()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Internsion Set - Scoped Variables provider", new IntensionalSet()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Syntactic Function - subexpression provider", new SyntacticFunctionsSubExpressionsProvider("type", "scoped variables")));
	}
	

	private static ImageIcon createImageIcon(String path) {
	    java.net.URL imgURL = AbstractRewritePanel.class.getResource(path);
	    return new ImageIcon(imgURL);
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
	
	/** Writes everything into the text area. */
	private class ConsoleOutputStream extends java.io.OutputStream {
		@Override
		public void write(int b) throws java.io.IOException {
			String s = new String(new char[] { (char) b });
			consoleOutputTextArea.append(s);
		}
	}
	
	private class RewriteUndoableEditListener implements UndoableEditListener {
		
		@Override
		public void undoableEditHappened(UndoableEditEvent e) {
			// Remember the edit and update the menus
			undoManager.addEdit(e.getEdit());
			btnUndo.setEnabled(undoManager.canUndo());
			btnRedo.setEnabled(undoManager.canRedo());
		}
	}
}
