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

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.demo.model.EnableItem;
import com.sri.ai.grinder.demo.model.ExampleRewrite;
import com.sri.ai.grinder.demo.model.GroupEnableItem;
import com.sri.ai.grinder.demo.model.LeafEnableItem;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseBranchesAreBooleanConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalization;
import com.sri.ai.grinder.library.controlflow.IfThenElseIrrelevantCondition;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.FromConditionalFormulaToFormula;
@Beta
public class ControlFlowRewritePanel extends AbstractRewritePanel {
	private static final long serialVersionUID = 1L;

	@Override
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			// Basic
			new ExampleRewrite("Known Condition", "if true then X = a else X = b"),
			new ExampleRewrite("Irrelevant Condition", "if X = a then Y = a else Y = a"),
			new ExampleRewrite("Boolean Constant Branches", "if X = a then false else true"),
			new ExampleRewrite("Condition matches Branches", "if even(X) then f(even(X)) else g(even(X))"),
			new ExampleRewrite("Externalization", "+(if X = a then 1 else 2, if Y = b then 3 else 4)"),
			new ExampleRewrite("Conditional Formula", "if X = a then Y = b else Z = c"),
		};
	}
	
	@Override
	protected EnableItem<Rewriter> getExampleRewriters() {

		List<EnableItem<Rewriter>> basicRewriters = new ArrayList<EnableItem<Rewriter>>();
		basicRewriters.add(new LeafEnableItem<Rewriter>("Known Condition",  new IfThenElse()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Irrelevant Condition",  new IfThenElseIrrelevantCondition()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Branches are Boolean Constants",  new IfThenElseBranchesAreBooleanConstants()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Condition matches branches",  new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Externalization",  new IfThenElseExternalization()));
		GroupEnableItem<Rewriter> basicGroup = new GroupEnableItem<Rewriter>("Basic", basicRewriters);
		
		List<EnableItem<Rewriter>> advancedRewriters = new ArrayList<EnableItem<Rewriter>>();
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Conditional Formula",  new FromConditionalFormulaToFormula()));
		GroupEnableItem<Rewriter> advancedGroup = new GroupEnableItem<Rewriter>("Advanced", advancedRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(basicGroup);
		groups.add(advancedGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Logic Rewriters", groups);
				
		return root; 
	}
}
