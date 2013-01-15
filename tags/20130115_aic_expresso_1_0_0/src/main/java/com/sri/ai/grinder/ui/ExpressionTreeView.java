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

import java.awt.event.MouseEvent;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JTree;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.google.common.annotations.Beta;

/**
 * 
 * @author braz
 *
 */
@Beta
public class ExpressionTreeView extends JTree implements TreeSelectionListener, TreeExpansionListener {
	private static final long serialVersionUID = 1L;
	private Set<TreePath> expandedPaths = null;
	private TreePath lastCollapsed = null;
	private boolean supressExpansionEvent = false;
	
	public ExpressionTreeView(ExpressionNode node, boolean keepExpandedNodesOpen) {
		super(node);
		init(keepExpandedNodesOpen);
	}
	
	public ExpressionTreeView(DefaultTreeModel model, boolean keepExpandedNodesOpen) {
		super(model);
		init(keepExpandedNodesOpen);
	}	

	private void init(boolean keepExpandedNodesOpen) {
		expandedPaths = Collections.synchronizedSet(new HashSet<TreePath>());
		setCellRenderer(new ExpressionNodeRenderer());
		getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		addTreeSelectionListener(this);
		if ( keepExpandedNodesOpen )
			addTreeExpansionListener(this);
		supressExpansionEvent = false;
	}
	
	
	public String getToolTipText(MouseEvent evt) {
        if (getRowForLocation(evt.getX(), evt.getY()) == -1)
          return null;
        TreePath curPath = getPathForLocation(evt.getX(), evt.getY());
        return ((ExpressionNode) curPath.getLastPathComponent()).getToolTipText();
      }

	@Override
	public void valueChanged(TreeSelectionEvent e) {
		//ExpressionNode node = (ExpressionNode)getLastSelectedPathComponent();
		//if (node == null) return;
	}


	@Override
	public void treeCollapsed(TreeExpansionEvent e) {
		if ( !supressExpansionEvent ) {
			lastCollapsed = e.getPath();
			expandedPaths.remove(lastCollapsed);
		}
	}

	@Override
	public void treeExpanded(TreeExpansionEvent e) {
		if ( !supressExpansionEvent ) {
			TreePath path = e.getPath();
			expandedPaths.add(path);
			if ( path.equals(lastCollapsed) ) {
				lastCollapsed = null;
			}
		}
	}	
	
	public void restoreExpandedPaths() {
		supressExpansionEvent = true;
		if ( expandedPaths != null ) {
			for (TreePath path: expandedPaths) {
				if ( lastCollapsed == null || !lastCollapsed.isDescendant(path) ) { 
					expandPath(path);
				}
			}
		}
		supressExpansionEvent = false;
	}
	
}
