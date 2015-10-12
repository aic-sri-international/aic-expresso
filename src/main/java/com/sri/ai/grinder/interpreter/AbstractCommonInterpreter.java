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

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.library.FunctorConstants.MAX;
import static com.sri.ai.grinder.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.library.CommonSimplifier;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.plaindpll.group.BooleansWithConjunctionGroup;
import com.sri.ai.grinder.plaindpll.group.BooleansWithDisjunctionGroup;
import com.sri.ai.grinder.plaindpll.group.SymbolicMaxGroup;
import com.sri.ai.grinder.plaindpll.group.SymbolicPlusGroup;
import com.sri.ai.grinder.plaindpll.group.SymbolicTimesGroup;

/**
 * An implementation of {@link AbstractInterpreter} re-using {@link CommonSimplifier}
 * (provided through {@link #makeAnotherMapBasedSimplifier()},
 * and delegating quantified expressions to abstract method
 * {@link AbstractCommonInterpreter#evaluateAggregateOperation(
 * AssociativeCommutativeGroup, ExtensionalIndexExpressionsSet, Expression, Expression, RewritingProcess)}.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractCommonInterpreter extends AbstractInterpreter {

	/**
	 * Constructs {@link AbstractCommonInterpreter}
	 * <i>not</i> simplifying literals according to contextual constraint.
	 */
	public AbstractCommonInterpreter() {
		this(false);
	}
	
	/**
	 * Constructs {@link AbstractCommonInterpreter} and
	 * sets it to simplify literals according to contextual constraint stored in
	 * <code>process</code>'s global object under {@link #INTERPRETER_CONTEXTUAL_CONSTRAINT}.
	 * @param simplifyGivenConstraint
	 */
	public AbstractCommonInterpreter(boolean simplifyGivenConstraint) {
		super(simplifyGivenConstraint);
	}
	
	public Map<String, Simplifier> makeFunctionApplicationSimplifiers() {
		return map(
				SUM,     (Simplifier) (s, p) -> evaluateAggregateOverIntensionalSet(new SymbolicPlusGroup(),  s, p),
				PRODUCT, (Simplifier) (s, p) -> evaluateAggregateOverIntensionalSet(new SymbolicTimesGroup(), s, p),
				MAX,     (Simplifier) (s, p) -> evaluateAggregateOverIntensionalSet(new SymbolicMaxGroup(),   s, p)
				);
	}

	public Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		return map(
				"There exists", (Simplifier) (s, p) -> evaluateQuantifiedExpression(s, new BooleansWithDisjunctionGroup(), p),
				"For all",      (Simplifier) (s, p) -> evaluateQuantifiedExpression(s, new BooleansWithConjunctionGroup(), p)
				);
	}

	private Expression evaluateAggregateOverIntensionalSet(AssociativeCommutativeGroup group, Expression expression, RewritingProcess process) throws Error {
		IntensionalSet intensionalSet = (IntensionalSet)expression.get(0);
		ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) intensionalSet.getIndexExpressions();
		// the set is intensional, but not the set of index expressions!
		Expression result =
				evaluateAggregateOperation(
						group, indexExpressions, intensionalSet.getCondition(), intensionalSet.getHead(), process);
		return result;
	}

	private Expression evaluateQuantifiedExpression(Expression expression, AssociativeCommutativeGroup group, RewritingProcess process) {
		QuantifiedExpressionWithABody quantifiedExpression = (QuantifiedExpressionWithABody) expression;
		Expression body = quantifiedExpression.getBody();
		ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) quantifiedExpression.getIndexExpressions();
		Expression result = evaluateAggregateOperation(group, indexExpressions, TRUE, body, process);
		return result;
	}

	/**
	 * Extensions can use this method to define how a aggregate or quantified expression is to be solved.
	 * @param group
	 * @param indexExpressions
	 * @param indicesCondition
	 * @param body
	 * @param process
	 * @return
	 * @throws Error
	 */
	abstract protected Expression evaluateAggregateOperation(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesCondition,
			Expression body,
			RewritingProcess process) throws Error;

	@Override
	public MapBasedSimplifier makeAnotherMapBasedSimplifier() {
		return new CommonSimplifier();
	}
}