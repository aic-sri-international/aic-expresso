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

import java.awt.Color;
import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;

import com.google.common.annotations.Beta;

/**
 * 
 * @author braz
 *
 */
@Beta
public class ExpressionNodeRenderer extends DefaultTreeCellRenderer {
	private static final long serialVersionUID = 1L;
	// private static final Color DARKBLUE  = new Color(51, 0, 153);
	// private static final Color DARKGREEN = new Color(0, 139, 69);
	private static final Color BROWN = new Color(139, 69, 19);
	
	public ExpressionNodeRenderer() {
		setClosedIcon(null);
	    setOpenIcon(null);
	    setLeafIcon(null);
	}
	  
	@Override
	public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {
		ExpressionNode node = (ExpressionNode) value;
		Object valueToDisplay = node;
		if (expanded && !leaf) {
			if (node.getType() != ExpressionNode.NodeType.STRING) {
				// If not a string and is expanded but not a leaf, then we don't want to see
				// the expression displayed as its individual parts will be displayed
				valueToDisplay = " ";
			}
		}	
		Component component = super.getTreeCellRendererComponent(tree, valueToDisplay, selected, expanded, leaf, row, hasFocus);
		if ( component instanceof JLabel ) {
			JLabel label = ((JLabel)component);
			ExpressionNode.NodeType type = node.getType();
			if ( type == null ) {
				label.setForeground(Color.BLACK);				
			} 
			else {
				switch ( type ) {
				case TERMINAL:
					label.setForeground(Color.GRAY);
					break;
				case SYMBOL:
					label.setForeground(Color.BLUE);
					break;
				case STRING:
					label.setForeground(BROWN);
					break;
				//case INARGUMENT:
				//	label.setForeground(Color.GRAY);
				//	break;
				//case FUNCTOR:
				//	label.setForeground(DARKGREEN);
				//	break;
				//case INFUNCTOR:
				//	label.setForeground(Color.GREEN);
				//	break;
				default:
					label.setForeground(Color.BLACK);
				}
			}

		}
		return component;
	}
	
}
