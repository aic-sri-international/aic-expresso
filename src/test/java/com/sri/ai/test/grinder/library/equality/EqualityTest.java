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
package com.sri.ai.test.grinder.library.equality;

import java.util.List;

import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.grinder.api.Library;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultLibrary;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.ExhaustiveRewriter;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.boole.ThereExistsSubExpressionsAndScopedVariablesProvider;
import com.sri.ai.grinder.library.equality.SolverLibrary;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class EqualityTest extends AbstractGrinderTest {

	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}

	/**
	 * @return
	 */
	public Library usedLibrary() {
		return new SolverLibrary();
	}

	@Test
	public void testConjunctionOfDisequalities() {
		List<Rewriter> rewriters = usedLibrary();
		evaluator = new ExhaustiveRewriter(rewriters);

		expressionString = "X != Y and Y != a";
		expected = parse("X != Y and Y != a");
		evaluationTest();

		expressionString = "X != Y and Z != b and Y != a";
		expected = parse("X != Y and Z != b and Y != a");
		evaluationTest();
		// The polish notation prevents associating the predicate,
		// which happens automatically upon parsing of infix "and".
		// This matters because the cluster is separated from
		// Z != b.

		expressionString = "X != Y and Z != b and Y != a and X != a and Y != b";
		expected = parse("and(X != Y, Z != b, Y != a, X != a, Y != b)");
		evaluationTest();
	}
	
	@Test
	public void testThereExistsSubExpressionsAndScopedVariablesProvider() {
		List<Rewriter> rewriters = new DefaultLibrary(
				new Equality(),
				new ExpressionKnowledgeModule(),
				new ThereExistsSubExpressionsAndScopedVariablesProvider(),
				new ScopedVariables()
		);

		evaluator = new ExhaustiveRewriter(rewriters);

		expressionString = "if Z = a then f(there exists Z : Z = a, Z = W) else 0";
		expected = parse("if Z = a then f(there exists Z : Z = a, Z = W) else 0");
		evaluationTest();
		
		expressionString = "'scoped variables'(there exists Z : Z)";
		expected = parse("list(Z)");
		evaluationTest();
		
		expressionString = "'scoped variables'(there exists f(X) : f(X))";
		expected = parse("list(f(X))");
		evaluationTest();
		
		expressionString = "if X = a then there exists f(X) : f(X) else 1";
		expected = parse("if X = a then there exists f(X) : f(X) else 1");
		evaluationTest();
	}
}
