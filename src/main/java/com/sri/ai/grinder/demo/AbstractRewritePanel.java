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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.undo.StateEdit;
import javax.swing.undo.StateEditable;
import javax.swing.undo.UndoManager;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.OpenInterpretationModule;
import com.sri.ai.grinder.core.RewriteOnce;
import com.sri.ai.grinder.demo.common.RewriterEnableTreeCellEditor;
import com.sri.ai.grinder.demo.common.RewriterEnableTreeRenderer;
import com.sri.ai.grinder.demo.model.EnableItem;
import com.sri.ai.grinder.demo.model.ExampleRewrite;
import com.sri.ai.grinder.demo.model.GroupEnableItem;
import com.sri.ai.grinder.demo.model.LeafEnableItem;
import com.sri.ai.grinder.demo.model.Options;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.AbsorbingElement;
import com.sri.ai.grinder.library.Associative;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.Distributive;
import com.sri.ai.grinder.library.PlainSubstitution;
import com.sri.ai.grinder.library.SyntacticFunctionsSubExpressionsProvider;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.controlflow.ImposedConditionsModule;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierElimination;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierEliminationWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplify;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.intensional.IntensionalSetWithFalseConditionIsEmptySet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.util.Configuration;

@Beta
public class AbstractRewritePanel extends JPanel {

	private static final long serialVersionUID = 1L;
	//
	private static final String ICON_RESOLUTION = "22x22";
	private static final String REWRITE_SEPARATOR = "------------";
	private static final String NO_FUTHER_SIMPLIFICATION_POSSIBLE = "No further possible simplification";
	
	//
	private Options          options               = null;
	private InputUndoManager undoManager           = new InputUndoManager();
	private int              singleStepCount       = 0;
	private InputStateEdit   currentInputStateEdit = null;
	private String           currentContext        = "";
	private String           currentInput          = "";
	private String           currentOutput         = "";
	//
	private DefaultMutableTreeNode exampleRewritersRootNode = null;
	private List<EnableItem<Rewriter>> rewriterEnableList   = new ArrayList<EnableItem<Rewriter>>();
	// 
	private ImageIcon imageUndoToInitialStep = createImageIcon("media-seek-backward"+ICON_RESOLUTION+".png");
	private ImageIcon imageUndoSingleStep    = createImageIcon("media-skip-backward"+ICON_RESOLUTION+".png");
	private ImageIcon imageStep              = createImageIcon("media-skip-forward"+ICON_RESOLUTION+".png");
	private ImageIcon imageExhaustive        = createImageIcon("media-seek-forward"+ICON_RESOLUTION+".png");
	private ImageIcon imageClear             = createImageIcon("edit-clear"+ICON_RESOLUTION+".png");
	//
	private ExpressionEditor inputContextExpressionEditor;
	private ExpressionEditor inputExpressionEditor;
	private ExpressionEditor outputExpressionEditor;
	private JComboBox exampleComboBox;
	private JTree rewriterEnableTree;
	private JPanel optionsPanel;
	private OutputPanel outputPanel;
	private JButton btnUndoSingleStep;
	private JButton btnUndoToInitialStep;

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
		
		btnUndoToInitialStep = new JButton("");
		btnUndoToInitialStep.setEnabled(false);
		actionButtonsPanel.add(btnUndoToInitialStep);
		btnUndoToInitialStep.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				handleUndo(true);
			}
		});
		btnUndoToInitialStep.setToolTipText("Undo input to initial step");
		btnUndoToInitialStep.setIcon(imageUndoToInitialStep);
		btnUndoToInitialStep.setText("");
		
		btnUndoSingleStep = new JButton("");
		btnUndoSingleStep.setEnabled(false);
		actionButtonsPanel.add(btnUndoSingleStep);
		btnUndoSingleStep.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				handleUndo(false);
			}
		});
		btnUndoSingleStep.setToolTipText("Undo input by a single step");
		btnUndoSingleStep.setIcon(imageUndoSingleStep);
		btnUndoSingleStep.setText("");
		
		JButton btnRewriteSingle = new JButton("| >");
		actionButtonsPanel.add(btnRewriteSingle);
		btnRewriteSingle.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				performRewrite(false);
			}
		});
		btnRewriteSingle.setToolTipText("Single rewrite step");
		btnRewriteSingle.setIcon(imageStep);
		btnRewriteSingle.setText("");
		
		JButton btnRewriteExhaustive = new JButton("->");
		actionButtonsPanel.add(btnRewriteExhaustive);
		btnRewriteExhaustive.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				performRewrite(true);
			}
		});
		btnRewriteExhaustive.setToolTipText("Exhaustive Rewrite");
		btnRewriteExhaustive.setIcon(imageExhaustive);
		btnRewriteExhaustive.setText("");
		
		JButton btnClear = new JButton("Clear");
		actionButtonsPanel.add(btnClear);
		btnClear.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				outputPanel.clearAllOutputs();
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
			@Override
			public void actionPerformed(ActionEvent arg0) {
				ExampleRewrite eg = (ExampleRewrite) exampleComboBox.getItemAt(exampleComboBox.getSelectedIndex());
				if (eg != null) {
					inputContextExpressionEditor.setText(eg.getInputContext());
					inputExpressionEditor.setText(eg.getInputExpression());
					outputExpressionEditor.setText("");
					trackUndoState(true);
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
		inputExpressionEditor.addFocusListener(new FocusAdapter() {
			private String focusGainedState = "";
			@Override
			public void focusGained(FocusEvent e) {
				focusGainedState = inputExpressionEditor.getText();
			}
			
			@Override
			public void focusLost(FocusEvent e) {
				String current = inputExpressionEditor.getText();
				if (!current.equals(focusGainedState)) {
					trackUndoState(true);
				}
			}
		});
		expressionInputPanel.add(inputExpressionEditor, BorderLayout.CENTER);
		
		JPanel inputContextPanel = new JPanel();
		expressionInputPanel.add(inputContextPanel, BorderLayout.NORTH);
		inputContextPanel.setLayout(new BorderLayout(0, 0));
		
		JLabel lblNewLabel = new JLabel("Context:");
		inputContextPanel.add(lblNewLabel, BorderLayout.WEST);
		
		inputContextExpressionEditor = new ExpressionEditor();
		inputContextExpressionEditor.addFocusListener(new FocusAdapter() {
			private String focusGainedState = "";
			@Override
			public void focusGained(FocusEvent e) {
				focusGainedState = inputContextExpressionEditor.getText();
			}
			
			@Override
			public void focusLost(FocusEvent e) {
				String current = inputContextExpressionEditor.getText();
				if (!current.equals(focusGainedState)) {
					trackUndoState(true);
				}
			}
		});
		inputContextPanel.add(inputContextExpressionEditor);
		
		JPanel expressionOutputPanel = new JPanel();
		expressionOutputPanel.setBorder(new TitledBorder(null, "Output", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		expressionViews.add(expressionOutputPanel);
		expressionOutputPanel.setLayout(new BorderLayout(0, 0));
		
		
		outputExpressionEditor = new ExpressionEditor();
		outputExpressionEditor.setEditable(false);
		expressionOutputPanel.add(outputExpressionEditor, BorderLayout.CENTER);
		
		JPanel outputContextPanel = new JPanel();
		outputExpressionEditor.add(outputContextPanel, BorderLayout.NORTH);
		outputContextPanel.setLayout(new BorderLayout(0, 0));
		
		outputPanel = new OutputPanel();
		outputPanel.setPreferredSize(new Dimension(300, 60));
		expressionAndOutputSplitPane.setRightComponent(outputPanel);
		
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
		outputPanel.notifySelected();
	}
	
	public void notifyUnselected() {
		outputPanel.notifyUnselected();
	}
	
	public PrintStream getConsoleOutputPrintStream() {
		return outputPanel.getConsoleOutputPrintStream();
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
	}
	
	private void performRewrite(boolean exhaustiveRewrite) {	
		Parser parser  = new AntlrGrinderParserWrapper();
		String context = inputContextExpressionEditor.getText();
		if (context.trim().length() == 0) {
			// The empty string is considered equivalent to true.
			context = "true";
		}
		Expression inputContext = parser.parse(context);
		if (inputContext == null) {
			outputExpressionEditor.setText("ERROR: Malformed Input Context.");
		}
		
		if (exhaustiveRewrite) {
			singleStepCount = 0;
		} 
		else {
			singleStepCount++;
		}
	
		if (singleStepCount > 1 && !currentInput.equals(currentOutput)) {
			currentInput = outputExpressionEditor.getText();
			inputExpressionEditor.setText(currentInput);
		}
		Expression input = parser.parse(inputExpressionEditor.getText());
		if (input == null) {
			outputExpressionEditor.setText("ERROR: Malformed Input Expression.");
		}
		if (inputContext  != null && input != null) {
			List<Rewriter> rewriters = getRewritersAndModules();
			RewritingProcess process = new DefaultRewritingProcess(Expressions.TRUE, new RewriteOnce(rewriters));
			
			String outputPrefix = "";
			if ( ! inputContext.equals(Expressions.TRUE) && ! FormulaUtil.isFormula(inputContext, process)) {
					outputPrefix = "// WARNING: Input Context is not a Formula (defaulting to true).\n";
			}
			
			process = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(Tuple.make(input, inputContext), process);
			process = GrinderUtil.extendContextualConstraint(inputContext, process);
			
			if (isCardinalityRewriterLookupNeeded(rewriters)) {
				((DefaultRewritingProcess)process).setRewriterLookup(new DefaultRewriterLookup(DirectCardinalityComputationFactory.getCardinalityRewritersMap()));
			}
			Configuration.setProperty(GrinderConfiguration.KEY_ASSUME_DOMAIN_ALWAYS_LARGE, ""+getOptions().isAssumeDomainsAlwaysLarge());
			CardinalityTypeOfLogicalVariable.registerTypeSizeOfLogicalVariableWithProcess(new CardinalityTypeOfLogicalVariable.TypeSizeOfLogicalVariable() {
				@Override
				public Integer size(Expression logicalVariable, RewritingProcess process) {
					Integer result = null; // unknown by default
					if (getOptions().isTypeSizeKnown()) {
						result = getOptions().getTypeSize();
					}
					return result;
				}
			}, process);
			
			Rewriter rewriter = new RewriteOnce(getRewritersAndModules());
			
			boolean exitRewriteLoop = false;
			boolean first = true;
			do {
				try {	
					if (!first) {
						currentInput = input.toString();
						inputExpressionEditor.setText(currentInput);
						// Note: Re-parsing the writer.toString version of the expression
						// ensure we process exhaustive steps the same way as single steps
						// as the writer toString logic can end up partially simplifying
						// expressions.
						input = parser.parse(currentInput);
					}
					
					System.out.println(REWRITE_SEPARATOR);
					Expression output = rewriter.rewrite(input, process);	
					System.out.println("");
					
					if (first && output == input) {
						exitRewriteLoop = true;
						if (singleStepCount > 1) {
							trackUndoState(false, currentContext, currentInput, currentOutput);
						}
						singleStepCount = 0;
						System.out.println(NO_FUTHER_SIMPLIFICATION_POSSIBLE);
					}
					else {
						currentOutput = output.toString();
						outputExpressionEditor.setText(currentOutput);
						
						trackUndoState(false, currentContext, currentInput, currentOutput);
						
						if ((!exitRewriteLoop && output == input) || !exhaustiveRewrite) {							
							exitRewriteLoop = true;
							if (exhaustiveRewrite) {
								System.out.println(NO_FUTHER_SIMPLIFICATION_POSSIBLE);
							}
						}
						input = output;
					
						try {
							outputExpressionEditor.setText(outputPrefix + output);					
						} catch (RuntimeException ire) {
							outputExpressionEditor.setText("// ERROR: Rewriting Output - \n"+output);
							ire.printStackTrace();
							exitRewriteLoop = true;
						}
					}
				} catch (RuntimeException ore) {
					outputExpressionEditor.setText("// ERROR: Rewriting Input - \n"+inputExpressionEditor.getText());
					ore.printStackTrace();
					exitRewriteLoop = true;
				}
				first = false;
			} while (!exitRewriteLoop);
		}
	}
	
	private void handleUndo(boolean toInitialStep) {
		
		if (undoManager.canUndo()) {
			undoManager.undoTo(toInitialStep);
			singleStepCount = 0;
		}
	}
	
	private void trackUndoState(boolean initialStep) {		
		trackUndoState(initialStep,
						inputContextExpressionEditor.getText(),
						inputExpressionEditor.getText(),
						outputExpressionEditor.getText());
	}
	
	private void trackUndoState(boolean isInitialStep, String context, String input, String output) {
		currentContext = context;
		currentInput   = input;
		currentOutput  = output;
		
		if (isInitialStep) {
			singleStepCount = 0;
		}
				
		if (currentInputStateEdit != null) {					
			currentInputStateEdit.end();
			undoManager.addEdit(currentInputStateEdit);
		}
		currentInputStateEdit = new InputStateEdit(new InputState(isInitialStep));
		
		if (undoManager.canUndo()) {
			btnUndoSingleStep.setEnabled(true);
			btnUndoToInitialStep.setEnabled(true);
		}
		else {
			btnUndoSingleStep.setEnabled(false);
			btnUndoToInitialStep.setEnabled(false);
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
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Imposed Conditions", new ImposedConditionsModule()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Open Interpretation Module", new OpenInterpretationModule()));
			
		// Providers
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("if . then . else - subexpression and imposed conditions provider", new IfThenElseSubExpressionsAndImposedConditionsProvider()));
		rewriterEnableList.add(new LeafEnableItem<Rewriter>("Syntactic Function - subexpression provider", new SyntacticFunctionsSubExpressionsProvider("type")));
	}
	

	private static ImageIcon createImageIcon(String path) {
	    java.net.URL imgURL = AbstractRewritePanel.class.getResource(path);
	    return new ImageIcon(imgURL);
	}
	
	private class InputUndoManager extends UndoManager {
		private static final long serialVersionUID = 1L;

		public void undoTo(boolean toInitialStep) {
			if (canUndo()) {
				if (toInitialStep) {
					InputStateEdit toBeUndone;
					do {
						toBeUndone = (InputStateEdit) editToBeUndone();
						undo();
					} while (canUndo() && !toBeUndone.isInitialStateEdit);
				} else {
					undo();
				}
			}
		}
	}
	
	private class InputState implements StateEditable {
		public static final String CONTEXT_EXPRESSION = "context";
		public static final String INPUT_EXPRESSION   = "input";
		public static final String OUTPUT_EXPRESSION  = "output";
		
		public boolean isInitialStateEdit;
		
		public InputState(boolean isInitialStateEdit) {
			this.isInitialStateEdit = isInitialStateEdit;
		}
		
		@Override
		public void storeState(Hashtable<Object,Object> state) {			
			state.put(CONTEXT_EXPRESSION, currentContext);
			state.put(INPUT_EXPRESSION,   currentInput);
			state.put(OUTPUT_EXPRESSION,  currentOutput);
		}
		
		@Override
		public void restoreState(Hashtable<?,?> state) {		
			if (state.containsKey(CONTEXT_EXPRESSION)) {
				currentContext = (String)state.get(CONTEXT_EXPRESSION);
				inputContextExpressionEditor.setText(currentContext);
			}
			if (state.containsKey(INPUT_EXPRESSION)) {
				currentInput = (String)state.get(INPUT_EXPRESSION);
				inputExpressionEditor.setText(currentInput);
			}
			if (state.containsKey(OUTPUT_EXPRESSION)) {
				currentOutput = (String)state.get(OUTPUT_EXPRESSION);
				outputExpressionEditor.setText(currentOutput);
			}
			currentInputStateEdit = null;
			trackUndoState(isInitialStateEdit, currentContext, currentInput, currentOutput);
		}
	}
	
	private class InputStateEdit extends StateEdit {
		private static final long serialVersionUID = 1L;
		//
		public boolean isInitialStateEdit;
		
		public InputStateEdit(InputState inputState) {
			super(inputState);
			this.isInitialStateEdit = inputState.isInitialStateEdit;
		}
	}
}
