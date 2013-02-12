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
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.NotOnDisequality;
import com.sri.ai.grinder.library.equality.NotOnEquality;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IncompleteLinearImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierEliminationWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplifyWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TrivialQuantifiedCases;

@Beta
public class LogicRewritePanel extends AbstractRewritePanel {
	private static final long serialVersionUID = 1L;

	@Override
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			// Basic
			new ExampleRewrite("And: true and x", "true and x"),
			new ExampleRewrite("And: and(true, false)", "and(true, false)"),
			new ExampleRewrite("Or: true or x", "true or x"),
			new ExampleRewrite("And Or", "and(x, or(y, z))"),
			new ExampleRewrite("Not: not(true)", "not(true)"),
			new ExampleRewrite("Implication: true => false", "true => false"),
			new ExampleRewrite("Implication: false => true", "false => true"),
			new ExampleRewrite("Equivalence: false <=> true", "false <=> true"),
			new ExampleRewrite("Equivalence: false <=> false", "false <=> false"),
			// Advanced
			new ExampleRewrite("Not Equality: not(X = a)", "not(X = a)"),
			new ExampleRewrite("Not Disequality: not(X != a)", "not(X != a)"),
			new ExampleRewrite("Linear Implied:", "if A = a then a = A else a != A"),
			new ExampleRewrite("There Exists: Trivial", "there exists X : true"),
			new ExampleRewrite("For All: Trivial", "for all X : false"),
		};
	}
	
	@Override
	protected EnableItem<Rewriter> getExampleRewriters() {

		List<EnableItem<Rewriter>> basicRewriters = new ArrayList<EnableItem<Rewriter>>();
		basicRewriters.add(new LeafEnableItem<Rewriter>("And",  new And()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Or",  new Or()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Not",  new Not()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Implication",  new Implication()));
		basicRewriters.add(new LeafEnableItem<Rewriter>("Equivalence",  new Equivalence()));
		GroupEnableItem<Rewriter> basicGroup = new GroupEnableItem<Rewriter>("Basic", basicRewriters);
		
		List<EnableItem<Rewriter>> advancedRewriters = new ArrayList<EnableItem<Rewriter>>();
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Not on Equality",  new NotOnEquality()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Not on Disequality",  new NotOnDisequality()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Incomplete Linear Implied Certainty", new IncompleteLinearImpliedCertainty()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Trivial Quantified Cases",  new TrivialQuantifiedCases()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Top Simplify",  new TopSimplifyWrapper()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Quantifier Elimination",  new QuantifierEliminationWrapper()));
		advancedRewriters.add(new LeafEnableItem<Rewriter>("Top Implied Certainty",  new TopImpliedCertainty()));
		GroupEnableItem<Rewriter> advancedGroup = new GroupEnableItem<Rewriter>("Advanced", advancedRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(basicGroup);
		groups.add(advancedGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Logic Rewriters", groups);
				
		return root; 
	}
}
