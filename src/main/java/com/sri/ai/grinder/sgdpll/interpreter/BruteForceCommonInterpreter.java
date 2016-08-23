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
package com.sri.ai.grinder.sgdpll.interpreter;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.grinder.helper.GrinderUtil.extendContextualSymbolsWithIndexExpressions;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.sgdpll.api.Context;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An extension of {@link AbstractCommonInterpreter}
 * that solves quantified and aggregate expressions by brute force.
 * <p>
 * Additionally, it takes an assignment to symbols as a constructing parameter,
 * and throws an error when a symbol with unassigned value is found.
 *
 * @author braz
 *
 */
@Beta
public class BruteForceCommonInterpreter extends AbstractCommonInterpreter {

	private Map<Expression, Expression> assignment;
	
	/**
	 * Constructs {@link BruteForceCommonInterpreter} with an empty initial assignment.
	 */
	public BruteForceCommonInterpreter() {
		this(map());
	}

	/**
	 * Constructs {@link BruteForceCommonInterpreter} with an initial assignment.
	 * @param assignment
	 */
	public BruteForceCommonInterpreter(Map<Expression, Expression> assignment) {
		super();
		this.assignment = assignment;
	}
	
	/**
	 * Creates a new interpreter with current assignment extended by new ones.
	 * @param extendingAssignment new value of assignments
	 * @param context the context
	 * @return
	 */
	public BruteForceCommonInterpreter extendWith(Map<Expression, Expression> extendingAssignment, Context context) {
		return new BruteForceCommonInterpreter(new StackedHashMap<>(extendingAssignment, assignment));
	}

	@Override
	public Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		Map<String, Simplifier> result = super.makeSyntacticFormTypeSimplifiers();
		result.put(
				"Symbol", (Simplifier) (s, p) -> {
					Expression symbolValue = assignment.get(s);
					if (symbolValue == null) {
						symbolValue = s;
					}
					return symbolValue;
				});
		return result;
	}

	@Override
	protected Expression evaluateAggregateOperation(
			AssociativeCommutativeGroup group, ExtensionalIndexExpressionsSet indexExpressions, Expression indicesCondition, Expression body, Context context) throws Error {
		
		context = extendContextualSymbolsWithIndexExpressions(indexExpressions, context);
		Expression value = group.additiveIdentityElement();
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(indexExpressions, context);
		for (Map<Expression, Expression> values : in(assignmentsIterator)) {
			Expression indicesConditionEvaluation = 
					evaluateGivenValuesAndCheckForBeingAConstant(indicesCondition, values, context);
			if (indicesConditionEvaluation.equals(FALSE)) {
				continue;
			}
			Expression bodyEvaluation = evaluateGivenValuesAndCheckForBeingAConstant(body, values, context);
			if (group.isAdditiveAbsorbingElement(bodyEvaluation)) {
				return bodyEvaluation;
			}
			value = group.add(value, bodyEvaluation, context);
		}
		return value;
	}

	private Expression evaluateGivenValuesAndCheckForBeingAConstant(Expression expression, Map<Expression, Expression> values, Context context) throws Error {
		Expression expressionEvaluation = extendWith(values, context).apply(expression, context);
		return expressionEvaluation;
	}
}