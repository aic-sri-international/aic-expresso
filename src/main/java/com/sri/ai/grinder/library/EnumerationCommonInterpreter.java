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
package com.sri.ai.grinder.library;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.extendContextualSymbolsWithIndexExpressions;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.plaindpll.group.BooleansWithConjunctionGroup;
import com.sri.ai.grinder.plaindpll.group.BooleansWithDisjunctionGroup;
import com.sri.ai.grinder.plaindpll.group.SymbolicPlusGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An implementation of {@link AbstractInterpreter} re-using {@link CommonSimplifier}
 * (provided by {@link #makeAnotherMapBasedSimplifier()},
 * and augmented with brute-force (that is, enumerative) solvers for
 * summations, and universal and existentially quantified formulas.
 * <p>
 * Additionally, it takes an assignment to symbols as a constructing parameter,
 * and throws an error when a symbol with unassigned value is found.
 *
 * @author braz
 *
 */
@Beta
public class EnumerationCommonInterpreter extends AbstractInterpreter {

	public static final String COMMON_INTERPRETER_ASSIGNMENT = "EnumerationCommonInterpreter assignment";
	
	private Map<Expression, Expression> assignment;
	
	/**
	 * Constructs {@link EnumerationCommonInterpreter} with an empty initial assignment and
	 * <i>not</i> simplifying literals according to contextual constraint.
	 */
	public EnumerationCommonInterpreter() {
		this(map());
	}

	/**
	 * Constructs {@link EnumerationCommonInterpreter} with an empty initial assignment and
	 * <simplifying literals according to contextual constraint.
	 */
	public EnumerationCommonInterpreter(boolean simplifyGivenConstraint) {
		this(map(), simplifyGivenConstraint);
	}

	/**
	 * Constructs {@link EnumerationCommonInterpreter} with an initial assignment and
	 * <i>not</i> simplifying literals according to contextual constraint.
	 * @param assignment
	 * @param simplifyGivenConstraint
	 */
	public EnumerationCommonInterpreter(Map<Expression, Expression> assignment) {
		this(assignment, false);
	}
	
	/**
	 * Constructs {@link EnumerationCommonInterpreter} with an initial assignment and
	 * sets it to simplify literals according to contextual constraint stored in
	 * <code>process</code>'s global object under {@link #INTERPRETER_CONTEXTUAL_CONSTRAINT}.
	 * @param assignment
	 * @param simplifyGivenConstraint
	 */
	public EnumerationCommonInterpreter(Map<Expression, Expression> assignment, boolean simplifyGivenConstraint) {
		super(simplifyGivenConstraint);
		this.assignment = assignment;
	}
	
	/**
	 * Creates a new interpreter with current assignment extended by new ones.
	 * @param extendingAssignment new value of assignments
	 * @param process the rewriting process
	 * @return
	 */
	public AbstractInterpreter extendWith(Map<Expression, Expression> extendingAssignment, RewritingProcess process) {
		return new EnumerationCommonInterpreter(new StackedHashMap<>(extendingAssignment, assignment), simplifyGivenConstraint);
	}

	public Map<String, Simplifier> makeFunctionApplicationSimplifiers() {
		return map(
				FunctorConstants.SUM, (Simplifier)
				(s, p) -> {
					IntensionalSet intensionalSet = (IntensionalSet)s.get(0);
					return evaluateGroupOperationForAllIndicesAssignmentOnBody(
							new SymbolicPlusGroup(),
							intensionalSet.getIndexExpressions(),
							intensionalSet.getCondition(),
							intensionalSet.getHead(),
							p);
				}
				);
	}

	public Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		return map(
				"Symbol", (Simplifier) (s, p) -> {
					Expression result = assignment.get(s);
					if (result == null) {
						result = s;
					}
					return result;
				},
				"There exists", (Simplifier) (s, p) -> evaluateQuantifiedExpression(s, new BooleansWithDisjunctionGroup(), p),
				"For all",      (Simplifier) (s, p) -> evaluateQuantifiedExpression(s, new BooleansWithConjunctionGroup(), p)
				);
	}

	@Override
	public MapBasedSimplifier makeAnotherMapBasedSimplifier() {
		return new CommonSimplifier();
	}

	private Expression evaluateQuantifiedExpression(Expression expression, AssociativeCommutativeGroup group, RewritingProcess process) {
		QuantifiedExpressionWithABody quantifiedExpression = (QuantifiedExpressionWithABody) expression;
		Expression body = quantifiedExpression.getBody();
		IndexExpressionsSet indexExpressions = quantifiedExpression.getIndexExpressions();
		return evaluateGroupOperationForAllIndicesAssignmentOnBody(group, indexExpressions, TRUE, body, process);
	}

	private Expression evaluateGroupOperationForAllIndicesAssignmentOnBody(
			AssociativeCommutativeGroup group, IndexExpressionsSet indexExpressions, Expression indicesCondition, Expression body, RewritingProcess process) throws Error {
		
		process = extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		Expression value = group.additiveIdentityElement();
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(indexExpressions, process);
		for (Map<Expression, Expression> values : in(assignmentsIterator)) {
			Expression indicesConditionEvaluation = evaluateGivenValuesAndCheckForBeingAConstant(indicesCondition, values, process);
			if (indicesConditionEvaluation.equals(FALSE)) {
				continue;
			}
			Expression bodyEvaluation = evaluateGivenValuesAndCheckForBeingAConstant(body, values, process);
			if (group.isAdditiveAbsorbingElement(bodyEvaluation)) {
				return bodyEvaluation;
			}
			value = group.add(value, bodyEvaluation, process);
		}
		return value;
	}

	private Expression evaluateGivenValuesAndCheckForBeingAConstant(Expression expression, Map<Expression, Expression> values, RewritingProcess process) throws Error {
		Expression expressionEvaluation = extendWith(values, process).apply(expression, process);
		if ( ! expressionEvaluation.getSyntacticFormType().equals("Symbol")) {
			throw new Error("Quantifier body must evaluate to constant but evaluated to " + expressionEvaluation);
		}
		return expressionEvaluation;
	}

	/**
	 * Simplifies a given expression with a {@link EnumerationCommonInterpreter} using enumeration under given contextual constraint.
	 * @param expression
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	public static Expression simplifyGivenContextualConstraint(Expression expression, Constraint contextualConstraint, RewritingProcess process) {
		AbstractInterpreter interpreter = new EnumerationCommonInterpreter(true /* simplify given contextual constraint */);
		Expression result = interpreter.simplifyUnderContextualConstraint(expression, contextualConstraint, process);
		return result;
	}

	public static void main(String[] args) {
		AbstractInterpreter interpreter = new EnumerationCommonInterpreter(map(parse("Hurrah"), parse("awesome")), true);
		RewritingProcess process = new DefaultRewritingProcess(null);
		Constraint contextualConstraint = new CompleteMultiVariableConstraint(new EqualityConstraintTheory());
		contextualConstraint = contextualConstraint.conjoin(parse("W != 3"), process);
		process.putGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT, contextualConstraint);
		process = process.put(new Categorical("Population", 5, arrayList(parse("tom")))); // two pitfalls: immutable process and need for arrayList rather than just list
		process = process.put(new Categorical("Numbers", 3, arrayList(parse("1"), parse("2"), parse("3"))));
//		Expression expression = parse("false and there exists X in Numbers : X = 3 and X + 1 = 1 + X");
		Expression expression = parse("if for all X in Population : (there exists Y in Population : Y != X and W != 3) then Hurrah else not Hurrah");
//		Expression expression = parse("there exists Y in Population : Y != tom and W != 3");
//		Expression expression = parse("sum({{(on Y in Population) 2 | Y != tom and W != 3}})");
		Expression result = interpreter.apply(expression, process);
		System.out.println("result: " + result);
	}
}