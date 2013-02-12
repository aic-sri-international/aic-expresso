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

import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import java.awt.BorderLayout;
import java.awt.Component;
import java.io.PrintStream;

import javax.swing.JTabbedPane;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.grinder.GrinderConfiguration;

import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class RewriteSystemDemoApp {

	private AbstractRewritePanel lastRewritePanelSelected;
	private PrintStream defaultOutStream;
	//
	private JFrame frmGrinderRewriteSystem;
	private JTabbedPane tabbedPane;
	private CardinalityOptionsPanel cardinalityOptionsPanel;
	private CardinalityRewritePanel cardinalityRewritePanel;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		// Ensure the grammar class passed in is used where necessary.
		BrewerConfiguration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, CommonGrammar.class.getName());

		EventQueue.invokeLater(new Runnable() {
			public void run() {				
				try {
					String configuredLookAndFeel = GrinderConfiguration.getDemoAppDefaultLookAndFeel();
				    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
				        if (configuredLookAndFeel.equals(info.getName())) {
				            UIManager.setLookAndFeel(info.getClassName());
				            break;
				        }
				    }
				} catch (Exception e) {
				    // If the configured look and feel is not available, the default system look and feel will remain in place.
				}
				
				try {
					RewriteSystemDemoApp window = new RewriteSystemDemoApp();
					window.frmGrinderRewriteSystem.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public RewriteSystemDemoApp() {
		initialize();
		postInitialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frmGrinderRewriteSystem = new JFrame();
		frmGrinderRewriteSystem.setTitle("Grinder: Rewriting System Demo");
		frmGrinderRewriteSystem.setBounds(100, 100, 860, 640);
		frmGrinderRewriteSystem.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JPanel backgroundPanel = new JPanel();
		frmGrinderRewriteSystem.getContentPane().add(backgroundPanel, BorderLayout.CENTER);
		backgroundPanel.setLayout(new BorderLayout(0, 0));
		
		tabbedPane = new JTabbedPane(JTabbedPane.TOP);
		tabbedPane.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent arg0) {
				// Ensure the standard output is set correctly based on the current tab.
				Component selectedTab = tabbedPane.getSelectedComponent();
				if (selectedTab instanceof AbstractRewritePanel) {
					AbstractRewritePanel currentRewritePanelSelected = (AbstractRewritePanel) selectedTab;
					
					currentRewritePanelSelected.notifySelected();
					System.setOut(currentRewritePanelSelected.getConsoleOutputPrintStream());
								
					if (lastRewritePanelSelected != null) {
						lastRewritePanelSelected.notifyUnselected();
					}
					lastRewritePanelSelected = currentRewritePanelSelected;
				}
				else {
					System.setOut(defaultOutStream);
					lastRewritePanelSelected = null;
				}
			}
		});
		backgroundPanel.add(tabbedPane, BorderLayout.CENTER);
		
		cardinalityOptionsPanel = new CardinalityOptionsPanel();
		
		JPanel arithmeticPanel = new ArithmeticRewritePanel();
		tabbedPane.addTab("Arithmetic", null, arithmeticPanel, null);
		
		JPanel equalityPanel = new EqualityRewritePanel();
		tabbedPane.addTab("Equality", null, equalityPanel, null);
		
		JPanel setsPanel = new SetsRewritePanel();
		tabbedPane.addTab("Sets", null, setsPanel, null);
		
		JPanel logicPanel = new LogicRewritePanel();
		tabbedPane.addTab("Logic", null, logicPanel, null);
		
		JPanel controlFlowPanel = new ControlFlowRewritePanel();
		tabbedPane.addTab("Control Flow", null, controlFlowPanel, null);
		
		cardinalityRewritePanel = new CardinalityRewritePanel();
		tabbedPane.addTab("Cardinality", null, cardinalityRewritePanel, null);
		cardinalityRewritePanel.setOptionsPanel(cardinalityOptionsPanel);
		
		JPanel allPanel = new AllRewritePanel();
		tabbedPane.addTab("All", null, allPanel, null);
	}
	
	private void postInitialize() {
		defaultOutStream = System.out;
		tabbedPane.setSelectedIndex(0);
	}

}
