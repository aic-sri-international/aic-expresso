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
import javax.swing.JCheckBox;
import java.awt.FlowLayout;
import javax.swing.JFormattedTextField;

import com.sri.ai.grinder.demo.model.Options;

import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.text.NumberFormat;

public class CardinalityOptionsPanel extends JPanel {
	
	private static final long serialVersionUID = 1L;
	//
	private Options options = null;
	private JCheckBox knownDomainSizeCheckBox;
	private JCheckBox assumeDomainAlwaysLargeCheckBox;
	private JFormattedTextField domainSizeTextField;
	/**
	 * Create the panel.
	 */
	public CardinalityOptionsPanel() {
		setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		
		JPanel panel = new JPanel();
		add(panel);
		panel.setLayout(new BorderLayout(0, 0));
		
		JPanel panel_1 = new JPanel();
		panel.add(panel_1, BorderLayout.NORTH);
		panel_1.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		
		knownDomainSizeCheckBox = new JCheckBox("Known Domain Size");
		knownDomainSizeCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				getOptions().setDomainSizeKnown(knownDomainSizeCheckBox.isSelected());
			}
		});
		panel_1.add(knownDomainSizeCheckBox);
		knownDomainSizeCheckBox.setSelected(true);
		
		domainSizeTextField = new JFormattedTextField(NumberFormat.getIntegerInstance());
		domainSizeTextField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				Integer size = new Integer(10);
				try {
					size = new Integer(domainSizeTextField.getText());
					if (size < 1) {
						size = 1;
						domainSizeTextField.setValue(size);
					}
				} catch (NumberFormatException nfe) {
					domainSizeTextField.setValue(size);
				}
				getOptions().setDomainSize(size);
			}
		});
		panel_1.add(domainSizeTextField);
		domainSizeTextField.setPreferredSize(new Dimension(80, 25));
		domainSizeTextField.setValue(new Integer(10));
		
		JPanel panel_2 = new JPanel();
		panel.add(panel_2, BorderLayout.CENTER);
		panel_2.setLayout(new BorderLayout(0, 0));
		
		JPanel panel_3 = new JPanel();
		FlowLayout flowLayout = (FlowLayout) panel_3.getLayout();
		flowLayout.setAlignment(FlowLayout.LEFT);
		panel_2.add(panel_3, BorderLayout.NORTH);
		
		assumeDomainAlwaysLargeCheckBox = new JCheckBox("Assume Domains Always Large");
		assumeDomainAlwaysLargeCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				getOptions().setAssumeDomainsAlwaysLarge(assumeDomainAlwaysLargeCheckBox.isSelected());
			}
		});
		panel_3.add(assumeDomainAlwaysLargeCheckBox);

	}
	
	public Options getOptions() {
		return options;
	}
	
	public void setOptions(Options options) {
		this.options = options;
		this.options.setDomainSizeKnown(knownDomainSizeCheckBox.isSelected());
		this.options.setDomainSize(new Integer(domainSizeTextField.getText()));
		this.options.setAssumeDomainsAlwaysLarge(assumeDomainAlwaysLargeCheckBox.isSelected());
	}
}
