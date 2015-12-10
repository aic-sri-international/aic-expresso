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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.interpreter.SGDPLLT.solve;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.CommonSimplifier;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;

/**
 * An extension of {@link AbstractInterpreter} re-using {@link CommonSimplifier}
 * (provided by {@link #makeAnotherMapBasedSimplifier()},
 * and augmented with symbolic solvers for
 * summations, and universal and existentially quantified formulas.
 *
 * @author braz
 *
 */
@Beta
public class SymbolicCommonInterpreter extends AbstractCommonInterpreter {

	private ConstraintTheory constraintTheory;
	
	/**
	 * Constructs {@link SymbolicCommonInterpreter} with a constraint theory and
	 * <i>not</i> simplifying literals according to contextual constraint.
	 * @param constraintTheory
	 */
	public SymbolicCommonInterpreter(ConstraintTheory constraintTheory) {
		this(constraintTheory, false);
	}

	/**
	 * Constructs {@link SymbolicCommonInterpreter} with a constraint theory and
	 * setting it to simplify literals according to contextual constraint stored in
	 * <code>process</code>'s global object under {@link #INTERPRETER_CONTEXTUAL_CONSTRAINT}.
	 * @param constraintTheory
	 * @param simplifyGivenConstraint
	 */
	public SymbolicCommonInterpreter(ConstraintTheory constraintTheory, boolean simplifyGivenConstraint) {
		super(constraintTheory, simplifyGivenConstraint);
		this.constraintTheory = constraintTheory;
	}
	
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	protected Expression evaluateAggregateOperation(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesConditions,
			Expression body,
			RewritingProcess process) throws Error {

		process = GrinderUtil.extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		
		// TODO: OPTIMIZATION: this *severely* slows down the algorithm
		// we need to organize things so that we do not depend of pre-simplifications like this
		// but do them once on the fly.
		Expression quantifierFreeBody = apply(body, process);
		Expression quantifierFreeIndicesCondition = apply(indicesConditions, process);

//		Expression quantifierFreeBody = body;
//		Expression quantifierFreeIndicesCondition = indicesConditions;

		Constraint2 contextualConstraint = 
				simplifyGivenConstraint?
						(Constraint2) process.getGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT)
						: new CompleteMultiVariableConstraint(constraintTheory);

		SimplifierUnderContextualConstraint simplifierUnderContextualConstraint =
				simplifyGivenConstraint?
						this
						: new SymbolicCommonInterpreter(constraintTheory, true);
		
		Expression result =
				solve(
						group,
						simplifierUnderContextualConstraint,
						indexExpressions,
						quantifierFreeIndicesCondition,
						quantifierFreeBody,
						contextualConstraint,
						process);
		
		return result;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		ConstraintTheory constraintTheory =
				new CompoundConstraintTheory(
						new EqualityConstraintTheory(true, true),
						new PropositionalConstraintTheory());
		
		AbstractInterpreter interpreter = new SymbolicCommonInterpreter(constraintTheory, true);
		
		RewritingProcess process = new DefaultRewritingProcess(null);
		process = process.put(new Categorical("Population", 1000, arrayList(parse("tom")))); // two pitfalls: immutable process and need for arrayList rather than just list
		process = process.put(new Categorical("Numbers", -1, arrayList(parse("1"), parse("2"), parse("3"))));

		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("W"), makeSymbol("Population")), process);
		
		Constraint2 contextualConstraint = new CompleteMultiVariableConstraint(constraintTheory);
		contextualConstraint = contextualConstraint.conjoin(parse("W != 3"), process);
		
		process.putGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT, contextualConstraint);
		
//		Expression expression = parse("there exists X in Numbers : X = 3 and X + 1 = 1 + X");
//		Expression expression = parse("if for all X in Population : (there exists Y in Population : Y != X and W != 3) then Hurrah else not Hurrah");
//		Expression expression = parse("there exists Y in Population : Y != tom and W != 3");
//		Expression expression = parse("sum({{(on Y in Population) 2 | for all X in Population : (X = tom) => Y != X and W != 3 and Z != 2}})");
//		Expression expression = parse("sum({{(on Y in Population, Z in Numbers) 2 | Y != tom and Z != 2}})");
		Expression expression = parse("sum({{(on Y in Population) sum({{(on Z in Numbers) 2 | Z != 2 and Z != 3}}) | Y != tom}})");
//		Expression expression = parse("sum({{(on Y in Population, Z in Numbers) 2 | (for all X in Population : (X = tom) => Y != X) and W != 3 and Z != 2}})");
//		Expression expression = parse("product({{(on Y in Population) 2 | Y != tom and W != 3}})");
//		Expression expression = parse("max({{(on Y in Population) 2 | for all X in Population : (X = tom) => Y != X and W != 3 and Z != 2}})");
		Expression result = interpreter.apply(expression, process);
		System.out.println("result: " + result);
	}
}