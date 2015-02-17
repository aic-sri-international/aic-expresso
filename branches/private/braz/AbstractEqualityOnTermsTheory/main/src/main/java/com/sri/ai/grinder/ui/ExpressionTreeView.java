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

import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.google.common.annotations.Beta;

/**
 * 
 * @author braz
 *
 */
@Beta
public class ExpressionTreeView extends JTree implements TreeExpansionListener {
	private static final long serialVersionUID = 1L;
	private Set<TreePath> expandedPaths = null;
	private TreePath lastCollapsed = null;
	private boolean supressExpansionEvent = false;
	//
	private JPopupMenu popupMenu = new JPopupMenu();
	private FindPanel  findPanel = null;
	
	public ExpressionTreeView(ExpressionNode node, boolean keepExpandedNodesOpen) {
		super(node);
		init(keepExpandedNodesOpen);
	}
	
	public ExpressionTreeView(DefaultTreeModel model, boolean keepExpandedNodesOpen) {
		super(model);
		init(keepExpandedNodesOpen);
	}	
	
	@Override
	public String getToolTipText(MouseEvent evt) {
        if (getRowForLocation(evt.getX(), evt.getY()) == -1) {
          return null;
        }
        
        TreePath curPath = getPathForLocation(evt.getX(), evt.getY());
        
        return ((ExpressionNode) curPath.getLastPathComponent()).getToolTipText();
    }
	
	public JPanel getFindPanel() {
		if (findPanel == null) {
			findPanel = new FindPanel();
			findPanel.setExpressionTreeView(this);
			// Invisible by default.
			findPanel.setVisible(false);
		}
		return findPanel;
	}
	
	public boolean findNext(String findWhat, boolean regularExpression) {
		TreePath startFrom = getLeadSelectionPath();
		if (startFrom == null) {
			startFrom = getRoot();
		}
		boolean result = search(startFrom, findWhat, true, regularExpression);
		return result;
	}
	
	public boolean findPrevious(String findWhat, boolean regularExpression) {
		TreePath startFrom = getLeadSelectionPath();
		if (startFrom == null) {
			startFrom = getLastLeaf();
		}
		boolean result = search(startFrom, findWhat, false, regularExpression);
		return result;
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
	
	//
	// PRIVATE 	
	//
	private void init(boolean keepExpandedNodesOpen) {
		expandedPaths = Collections.synchronizedSet(new LinkedHashSet<TreePath>());
		setCellRenderer(new ExpressionNodeRenderer());
		getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		if (keepExpandedNodesOpen) {
			addTreeExpansionListener(this);
		}
		supressExpansionEvent = false;

		initPopupMenu();
	}
	
	private void initPopupMenu() {
		JMenuItem goLast = new JMenuItem("Go to last line");
		goLast.setAction(new GoLastAction());
		popupMenu.add(goLast);
		
		JMenuItem goLastLineOfBranch = new JMenuItem("Go to last line of branch");
		goLastLineOfBranch.setAction(new GoLastLineOfBranchAction());
		popupMenu.add(goLastLineOfBranch);
		
		JMenuItem find = new JMenuItem("Find");
		find.setAction(new FindAction());
		popupMenu.add(find);
		
		//Add listener to components that can bring up popup menus.
	    MouseListener popupListener = new PopupListener();
	    addMouseListener(popupListener);
	}
	
	private TreePath getRoot() {
		TreeModel      model = getModel();
		List<TreeNode> path  = new ArrayList<TreeNode>();
		if (model.getRoot() != null) {
			path.add((TreeNode)model.getRoot());
		}

		TreePath treePath = new TreePath(path.toArray(new TreeNode[path.size()]));
		return treePath;
	}
	
	private TreePath getLastLeaf() {
		return getLastLeafFrom((TreeNode)getRoot().getLastPathComponent());
	}
	
	private TreePath getLastLeafFrom(TreeNode node) {
		TreePath treePath = getRoot();
		if (node != null) {
			treePath = makePath(node);
			
			TreeModel model = getModel();
			int childCount = model.getChildCount(node);
			while (childCount > 0) {
				node = (TreeNode) model.getChild(node, childCount-1);
				treePath = treePath.pathByAddingChild(node);
				childCount = model.getChildCount(node);
			}
		}

		return treePath;
	}
	
	private void gotoNode(TreePath path) {
		addSelectionPath(path);
		scrollPathToVisible(path);
	}
	
	private TreePath makePath(TreeNode toNode) {
		List<TreeNode> path  = new ArrayList<TreeNode>();
		path.add(toNode);
		while (toNode.getParent() != null) {
			path.add(0, toNode.getParent());
			toNode = toNode.getParent();
		}
		
		TreePath treePath = new TreePath(path.toArray(new TreeNode[path.size()]));
		return treePath;
	}
	
	private boolean search(TreePath startFromPath, String findWhat, boolean searchForward, boolean regularExpression) {
		boolean result = false;
		
		Pattern   pattern   = null;
		if (regularExpression) {
			pattern = Pattern.compile(findWhat);
		}
		TreeModel model     = getModel();
		if (model.getRoot() != null) {
			TreeNode root      = (TreeNode) getRoot().getLastPathComponent();
			TreeNode lastLeaf  = (TreeNode) getLastLeaf().getLastPathComponent();
			// Ensure more than just a root node in the tree
			if (root != lastLeaf) {
				TreeNode startFrom = (TreeNode) startFromPath.getLastPathComponent();			
				TreeNode current   = startFrom;
				boolean found = false;
				do {
					if (searchForward) {
						current = next(root, lastLeaf, current, current);
					}
					else {
						current = prev(root, lastLeaf, current, current);
					}
					
					if (regularExpression) {
						Matcher m = pattern.matcher(current.toString());
						found = m.find();
					}
					else {
						found = current.toString().contains(findWhat);
					}
					
					if (found) {
						result = true;
						gotoNode(makePath(current));
						startFrom = current;
					}
				} while (current != startFrom);
			}
		}
		
		return result;
	}
	
	// We walk through the tree in depth first order
	private TreeNode next(TreeNode root, TreeNode lastLeaf, TreeNode current, TreeNode previous) {
		TreeNode result = current;
		
		if (current == lastLeaf) {
			result = root;
		}
		else {
			// At a leaf
			if (current.getChildCount() == 0) {
				result = next(root, lastLeaf, current.getParent(), current);
			}
			else  {
				// Have children, fist check if previous in children
				boolean foundPrevious = false;
				for (int i = 0; i < current.getChildCount(); i++) {
					TreeNode child = current.getChildAt(i);
					if (child == previous) {
						foundPrevious = true;
					}
					else {
						if (foundPrevious) {
							result = child;
							break;
						}						
					}
				}
				// Did not find previous in the children, means first time visiting this parent's children
				if (!foundPrevious) {
					// Take the first one
					result = current.getChildAt(0);
				}
				else {
					if (result == current) {
						// Move up to the next level
						result = next(root, lastLeaf, current.getParent(), current);
					}
				}
			}
		}
		
		return result;
	}
	
	private TreeNode prev(TreeNode root, TreeNode lastLeaf, TreeNode current, TreeNode previous) {
		TreeNode result = current;
		
		if (current == root) {
			result = lastLeaf;
		}
		else {
			TreeNode parent = current.getParent();
			boolean foundCurrent = false;
			for (int i = parent.getChildCount()-1; i >= 0; i--) {
				TreeNode child = parent.getChildAt(i);
				if (child == current) {
					foundCurrent = true;
				}
				else {
					if (foundCurrent) {
						result = child;
						while (result.getChildCount() > 0) {
							result = result.getChildAt(result.getChildCount()-1);
						}
						break;
					}						
				}
			}
			// Means current is first child
			if (result == current) {
				// Move up to the next level
				result = parent;
			}
		}
		
		return result;
	}
	
	class PopupListener extends MouseAdapter {
	    @Override
		public void mousePressed(MouseEvent e) {
	        maybeShowPopup(e);
	    }

	    @Override
		public void mouseReleased(MouseEvent e) {
	        maybeShowPopup(e);
	    }

	    private void maybeShowPopup(MouseEvent e) {
	        if (e.isPopupTrigger()) {
	            popupMenu.show(e.getComponent(),
	                       e.getX(), e.getY());
	        }
	    }
	}
	
	class GoLastAction extends AbstractAction {
		private static final long serialVersionUID = 1L;
		
		public GoLastAction() {
			putValue(Action.NAME, "Go to last line");
			putValue(Action.SHORT_DESCRIPTION, "Go to last line, expanding all parent nodes in the process");
		}
		
		@Override
		public void actionPerformed(ActionEvent ae) {
			gotoNode(getLastLeaf());
		}
	}
	
	class GoLastLineOfBranchAction extends AbstractAction {
		private static final long serialVersionUID = 1L;
		
		public GoLastLineOfBranchAction() {
			putValue(Action.NAME, "Go to last line of branch");
			putValue(Action.SHORT_DESCRIPTION, "Go to last line of branch, expanding all parent nodes in the process");
		}
		
		@Override
		public void actionPerformed(ActionEvent ae) {
			TreePath startFrom = getLeadSelectionPath();
			if (startFrom == null) {
				gotoNode(getLastLeaf());
			}
			else {
				gotoNode(getLastLeafFrom((TreeNode)startFrom.getLastPathComponent()));
			}
		}
	}
	
	class FindAction extends AbstractAction {
		private static final long serialVersionUID = 1L;
		
		public FindAction() {
			putValue(Action.NAME, "Find...");
			putValue(Action.SHORT_DESCRIPTION, "Find...");
		}
		
		@Override
		public void actionPerformed(ActionEvent ae) {
			getFindPanel().setVisible(true);
			getFindPanel().invalidate();
		}
	}
}
