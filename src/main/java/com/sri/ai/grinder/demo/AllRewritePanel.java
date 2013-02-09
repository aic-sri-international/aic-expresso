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
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseBranchesAreBooleanConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalization;
import com.sri.ai.grinder.library.controlflow.IfThenElseIrrelevantCondition;
import com.sri.ai.grinder.library.equality.NotOnDisequality;
import com.sri.ai.grinder.library.equality.NotOnEquality;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.FromConditionalFormulaToFormula;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IncompleteLinearImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TrivialQuantifiedCases;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.library.set.extensional.UnionOnExtensionalSets;
import com.sri.ai.grinder.library.set.intensional.EqualityOfIntensionalUniSets;

@Beta
public class AllRewritePanel extends AbstractRewritePanel {
	private static final long serialVersionUID = 1L;

	@Override
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			// Example
			new ExampleRewrite("Example 1", "if A = b then 200/32 else 29^3"),
			new ExampleRewrite("Example 2", "{ (on X) Alpha | a = b }"),
			new ExampleRewrite("Example 3", "if { f(X) | X != 2 and 1 = 2 } = {} then yeah else nah"),
			new ExampleRewrite("Example 4", "{ 1, (if A = B then 2 + 4 else 3 * 9^3) }"),
			new ExampleRewrite("Example 5", "if (1+2) = 3 then X = a else Y = b"),
			new ExampleRewrite("Example 6", "if ((1+2) = 3) and ((2-1) = 3) then X = a else Y = b"),
		};
	}
	
	@Override
	protected EnableItem<Rewriter> getExampleRewriters() {

		List<EnableItem<Rewriter>> allRewriters = new ArrayList<EnableItem<Rewriter>>();

		allRewriters.add(new LeafEnableItem<Rewriter>("Plus",  new Plus()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Minus", new Minus()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Unary Minus", new UnaryMinus()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Times", new Times()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Division", new Division()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Exponentiation",  new Exponentiation()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Known Condition",  new IfThenElse()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Equality",  new Equality()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Disequality",  new Disequality()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Union on Extensional Sets",  new UnionOnExtensionalSets()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Equality of Intensional Uni-Set",  new EqualityOfIntensionalUniSets()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Not on Equality",  new NotOnEquality()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Not on Disequality",  new NotOnDisequality()));
		allRewriters.add(new LeafEnableItem<Rewriter>("And",  new And()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Or",  new Or()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Not",  new Not()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Implication",  new Implication()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Equivalence",  new Equivalence()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Conditional Formula",  new FromConditionalFormulaToFormula()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Incomplete Linear Implied Certainty", new IncompleteLinearImpliedCertainty()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Trivial Quantified Cases",  new TrivialQuantifiedCases()));		
		allRewriters.add(new LeafEnableItem<Rewriter>("Irrelevant Condition",  new IfThenElseIrrelevantCondition()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Branches are Boolean Constants",  new IfThenElseBranchesAreBooleanConstants()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Condition matches branches",  new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch()));
		allRewriters.add(new LeafEnableItem<Rewriter>("Externalization",  new IfThenElseExternalization()));
		GroupEnableItem<Rewriter> allGroup = new GroupEnableItem<Rewriter>("All", allRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(allGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("All Rewriters", groups);
				
		return root; 
	}
}
