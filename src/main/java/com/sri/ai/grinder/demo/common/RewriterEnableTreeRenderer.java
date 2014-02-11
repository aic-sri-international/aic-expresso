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
package com.sri.ai.grinder.demo.common;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.demo.model.EnableItem;

public class RewriterEnableTreeRenderer extends DefaultTreeCellRenderer {
	private static final long serialVersionUID = 1L;
	private Map<EnableItem<Rewriter>, JCheckBox> checkBoxes = new LinkedHashMap<EnableItem<Rewriter>, JCheckBox>();

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