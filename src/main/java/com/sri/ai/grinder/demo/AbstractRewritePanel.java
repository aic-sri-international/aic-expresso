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
import javax.swing.JSplitPane;
import java.awt.Dimension;
import javax.swing.JLabel;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import java.awt.FlowLayout;
import javax.swing.JTabbedPane;
import javax.swing.border.TitledBorder;

public class AbstractRewritePanel extends JPanel {

	private static final long serialVersionUID = 1L;

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
		expressionAndOutputSplitPane.setOneTouchExpandable(true);
		expressionAndOutputSplitPane.setResizeWeight(1.0);
		expressionAndOutputSplitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		mainSplitPane.setLeftComponent(expressionAndOutputSplitPane);
		
		JPanel expressionPanel = new JPanel();
		expressionPanel.setMinimumSize(new Dimension(400, 250));
		expressionAndOutputSplitPane.setLeftComponent(expressionPanel);
		expressionPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel examplePanel = new JPanel();
		FlowLayout flowLayout = (FlowLayout) examplePanel.getLayout();
		flowLayout.setAlignment(FlowLayout.LEFT);
		expressionPanel.add(examplePanel, BorderLayout.NORTH);
		
		JLabel exampleLabel = new JLabel("Example:");
		examplePanel.add(exampleLabel);
		
		JComboBox comboBox = new JComboBox();
		comboBox.setModel(new DefaultComboBoxModel(new String[] {"Example 1", "Example 2", "Example 3"}));
		examplePanel.add(comboBox);
		
		JPanel expressionEditor = new JPanel();
		expressionPanel.add(expressionEditor, BorderLayout.CENTER);
		
		JPanel outputPanel = new JPanel();
		expressionAndOutputSplitPane.setRightComponent(outputPanel);
		outputPanel.setLayout(new BorderLayout(0, 0));
		
		JTabbedPane outputPane = new JTabbedPane(JTabbedPane.TOP);
		outputPanel.add(outputPane, BorderLayout.NORTH);
		
		JPanel consolePanel = new JPanel();
		outputPane.addTab("Console", null, consolePanel, null);
		consolePanel.setLayout(new BorderLayout(0, 0));
		
		JPanel tracePanel = new JPanel();
		outputPane.addTab("Trace", null, tracePanel, null);
		
		JPanel controlPanel = new JPanel();
		controlPanel.setMinimumSize(new Dimension(200, 300));
		mainSplitPane.setRightComponent(controlPanel);
		controlPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel rewriterSelectionPanel = new JPanel();
		rewriterSelectionPanel.setBorder(new TitledBorder(null, "Rewriters", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		controlPanel.add(rewriterSelectionPanel, BorderLayout.CENTER);
		
		JPanel actionPanel = new JPanel();
		actionPanel.setBorder(new TitledBorder(null, "Action", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		controlPanel.add(actionPanel, BorderLayout.SOUTH);

	}

}
