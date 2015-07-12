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
package com.sri.ai.test.grinder.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Collection;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.plaindpll.theory.SingleVariableEqualityConstraint;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.base.Pair;

@Beta
public class SingleVariableEqualityConstraintTest {
	
	@Test
	public void test() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		Expression expectedConstraint;
		Collection<Expression> impliedLiterals;
		Pair<SingleVariableConstraint, Collection<Expression>> actual;
		RewritingProcess process = DPLLUtil.makeProcess(map("X", "MyType"), map("MyType", "3"));
		
		actual = make("X", "X = 10", process);
		expectedConstraint = parse("X = 10");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10", process);
		expectedConstraint = parse("X != 10");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X = 5", process);
		expectedConstraint = parse("X = 5");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X = 10 and X = Y", process);
		expectedConstraint = parse("X = 10");
		impliedLiterals = list(parse("10 = Y"));
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X = 10 and X = 8", process);
		expectedConstraint = null;
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X = 10", process);
		expectedConstraint = null;
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X = 8", process);
		expectedConstraint = parse("X = 8");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X = Y", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list(parse("Y != 10"));
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X != 11 and X = 8", process);
		expectedConstraint = parse("X = 8");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X != 11 and X = Y", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list(parse("Y != 10"), parse("Y != 11"));
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X != 10 and X != 11 and X != 12 and X = Y", process);
		expectedConstraint = null;
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X = Y and X != 10 and X != 11 and X != 12", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list(parse("Y != 10"), parse("Y != 11"), parse("Y != 12"));
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X = Y and a != a", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list(parse("a != a"));
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X = Y and true", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "X = Y and false", process);
		expectedConstraint = null;
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "a != a and X = Y", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list(parse("a != a"));
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "true and X = Y", process);
		expectedConstraint = parse("X = Y");
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
		
		actual = make("X", "false and X = Y", process);
		expectedConstraint = null;
		impliedLiterals = list();
		equals(expectedConstraint, impliedLiterals, actual);
	}

	/**
	 * @param expectedConstraint
	 * @param impliedLiterals
	 * @param actual
	 */
	protected void equals(Expression expectedConstraint, Collection<Expression> impliedLiterals, Pair<SingleVariableConstraint, Collection<Expression>> actual) {
		if (actual == null) {
			if (expectedConstraint == null) {
				
			}
			else {
				fail();
			}
		}
		else {
			assertEquals(expectedConstraint, actual.first);
			assertEquals(impliedLiterals, actual.second);
		}
	}

	/**
	 * @param variableString
	 * @param process TODO
	 * @return
	 */
	protected Pair<SingleVariableConstraint, Collection<Expression>> make(String variableString, String expressionString, RewritingProcess process) {
		Pair<SingleVariableConstraint, Collection<Expression>> result;
		
		Expression variable = parse(variableString);
		Expression expression = parse(expressionString);
		Collection<Expression> conjuncts = And.getConjuncts(expression);

		SingleVariableConstraint resultConstraint = new SingleVariableEqualityConstraint(variable);
		Collection<Expression> totalImpliedLiterals = list();
		for (Expression conjunct : conjuncts) {
			SingleVariableEqualityConstraint.ApplicationResult applicationResult = resultConstraint.incorporate(conjunct, process);
			if (applicationResult == null) {
				resultConstraint = null;
				break;
			}
			else {
				resultConstraint = applicationResult.getConstraint();
				totalImpliedLiterals.addAll(applicationResult.getImpliedLiterals());
			}
		}
		
		result = Pair.make(resultConstraint, totalImpliedLiterals);
		return result;
	}
}
