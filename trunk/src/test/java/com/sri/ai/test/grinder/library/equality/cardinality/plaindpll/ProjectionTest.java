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
package com.sri.ai.test.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.X;
import static com.sri.ai.expresso.helper.Expressions.Y;
import static com.sri.ai.expresso.helper.Expressions.Z;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.plaindpll.util.DPLLUtil.makeProcess;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;

@Beta
public class ProjectionTest {
	
	@Test
	public void test() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		List<Expression> totalIndices;
		List<Expression> projectionIndices;
		Expression expected;
		Expression projection; // Expression in general, but we want to test that it will actually be represented as a constraint.
		
		EqualityConstraintTheory theory = new EqualityConstraintTheory(new SymbolTermTheory());
		RewritingProcess process = makeProcess(theory, map("X", "Everything", "Y", "Everything", "Z", "Everything"), map("Everything", "10"));
		
		totalIndices = list(X, Y);
		Constraint constraint = theory.makeConstraint(totalIndices);
		constraint = constraint.incorporate(false, parse("X = Y"), process);
		constraint = constraint.incorporate(false, parse("Y = Z"), process);

		projectionIndices = list(X, Y);
		expected = parse("true");
		projection = constraint.project(projectionIndices, process);
		assertEquals(expected, projection);
		
		projectionIndices = list(X);
		expected = parse("if Y = Z then false else true");
		projection = constraint.project(projectionIndices, process);
		assertEquals(expected, projection);
		
		projectionIndices = list();
		expected = parse("if Y = Z then false else if X = Y then false else true");
		projection = constraint.project(projectionIndices, process);
		assertEquals(expected, projection);
		
		projectionIndices = list(Y, Z); // invalid because Z is not in total indices.
		projection = null; // need to erase previous values of projection in case an error is thrown
		try {
			projection = constraint.project(projectionIndices, process);
		} catch (AssertionError error) {
			assertTrue(error.getMessage().contains("indicesSubSet must be a sub-set of getIndices()"));
		}
		if (projection != null) {
			fail("An error about the projection indices needing to be a sub-set of the total indices should have been thrown");
		}
	}
}
