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
package com.sri.ai.test.grinder.library.equality.cardinality.direct;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalizationHierarchical;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.FormulaSimplify;

public class FormulaSimplifyTest {
	
	RewritingProcess process;
	
	FormulaSimplify simplifier = new FormulaSimplify();
	FormulaSimplify simplifierPreservingStructure = new FormulaSimplify(true);

	IfThenElseExternalizationHierarchical externalizer                    = new IfThenElseExternalizationHierarchical();
	IfThenElseExternalizationHierarchical externalizerPreservingStructure = new IfThenElseExternalizationHierarchical(true, true);

	Expression expression;
	Expression expected;
	Expression expectedWithStructure;

	Expression actual;
	Expression actualWithStructure;

	@Test
	public void testFormulaSimplify() {
		expression            = parse("if Y = p then true else false");
		expected              = parse("Y = p");
		expectedWithStructure = parse("Y = p"); // ok to remove trivial expressions
		runFormulaSimplifyTest();
		
		expression            = parse("if true then Y = p else false");
		expected              = parse("Y = p");
		expectedWithStructure = parse("Y = p"); // ok to remove trivial expressions
		runFormulaSimplifyTest();
		
		expression            = parse("if Y = p then if X = q then true else true else X != q");
		expected              = parse("Y = p or X != q"); // ok to collapse structure
		expectedWithStructure = parse("if Y = p then true else X != q"); // not ok to collapse structure
		runFormulaSimplifyTest();

		expression            = parse("if X = q then false else Y = p");
		expected              = parse("X != q and Y = p"); // ok to collapse structure
		expectedWithStructure = parse("if X = q then false else Y = p"); // not ok to collapse structure
		runFormulaSimplifyTest();
	}

	public void runFormulaSimplifyTest() {
		process = DirectCardinalityComputationFactory.newCardinalityProcess();
		process = GrinderUtil.extendContextualVariablesWithFreeVariablesInExpressionWithUnknownDomainForSetUpPurposesOnly(expression, process);
		actual = process.rewrite(simplifier, expression);
		assertEquals(expected, actual);
		actualWithStructure = process.rewrite(simplifierPreservingStructure, expression);
		assertEquals(expectedWithStructure, actualWithStructure);
	}

	@Test
	public void testIfThenElseExternalize() {
		expression            = parse("(if Y = p then true else false) or Y = p");
		expected              = parse("Y = p");
		expectedWithStructure = parse("Y = p");
		runIfThenElseExternalizeTest();

		expression            = parse("if (if Y = p then true else false) then if X = q then true else false else Y = p");
		expected              = parse("Y = p and X = q"); // ok to collapse structure
		expectedWithStructure = parse("if Y = p then X = q else false"); // not ok to collapse structure
		runIfThenElseExternalizeTest();
	}

	public void runIfThenElseExternalizeTest() {
		process = DirectCardinalityComputationFactory.newCardinalityProcess();
		process = GrinderUtil.extendContextualVariablesWithFreeVariablesInExpressionWithUnknownDomainForSetUpPurposesOnly(expression, process);
		actual = process.rewrite(externalizer, expression);
		assertEquals(expected, actual);
		actualWithStructure = process.rewrite(externalizerPreservingStructure, expression);
		assertEquals(expectedWithStructure, actualWithStructure);
	}
}