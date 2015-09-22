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
import com.sri.ai.expresso.api.ExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.UniversallyQuantifiedFormula;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.MergingMapBasedSimplifier;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An extension of {@link CommonSimplifier} associated with an assignment to {@link Symbol}s,
 * and augmented with elementary simplifiers that replace symbols by their value in the assignment, if present.
 * 
 * @author braz
 *
 */
@Beta
public class CommonInterpreter implements Simplifier {

	private Map<Expression, Expression> assignment;
	private Simplifier simplifier;
	
	public CommonInterpreter() {
		this(map());
	}

	public CommonInterpreter(Map<Expression, Expression> assignment) {
		this.assignment = assignment;
		this.simplifier = new MergingMapBasedSimplifier(
				makeFunctionApplicationSimplifiers(), makeSyntacticFormTypeSimplifiers(), new CommonSimplifier());
	}
	
	/**
	 * Creates a new interpreter with current assignment extended by new ones.
	 * @param extendingAssignment new value of assignments
	 * @return
	 */
	public CommonInterpreter extendWith(Map<Expression, Expression> extendingAssignment) {
		return new CommonInterpreter(new StackedHashMap<>(extendingAssignment, assignment));
	}

	public Map<Expression, Expression> getAssignment() {
		return assignment;
	}
	
	public Map<String, Simplifier> makeFunctionApplicationSimplifiers() {
		return map();
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
				"There exists", (Simplifier) (s, p) -> evaluateQuantifiedExpression(s, p),
				"For all",      (Simplifier) (s, p) -> evaluateQuantifiedExpression(s, p)
				);
	}

	private Expression evaluateQuantifiedExpression(Expression expression, RewritingProcess process) {
		QuantifiedExpressionWithABody formula = (QuantifiedExpressionWithABody) expression;
		process = extendContextualSymbolsWithIndexExpressions(formula.getIndexExpressions(), process);
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(formula.getIndexExpressions(), process);
		for (Map<Expression, Expression> values : in(assignmentsIterator)) {
			Expression bodyEvaluation = extendWith(values).apply(formula.getBody(), process);
			if (bodyEvaluation.equals(shortCircuitingValue(formula))) {
				return shortCircuitingValue(formula);
			}
		}
		return identityValue(formula);
	}

	private Expression shortCircuitingValue(QuantifiedExpression expression) {
		if (expression instanceof ExistentiallyQuantifiedFormula) {
			return TRUE;
		}
		else if (expression instanceof UniversallyQuantifiedFormula) {
			return FALSE;
		}
		throw new Error("shortCircuitingValue: unregistered quantified expression: " + expression);
	}

	private Expression identityValue(QuantifiedExpression expression) {
		if (expression instanceof ExistentiallyQuantifiedFormula) {
			return FALSE;
		}
		else if (expression instanceof UniversallyQuantifiedFormula) {
			return TRUE;
		}
		throw new Error("shortCircuitingValue: unregistered quantified expression: " + expression);
	}

	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
		return simplifier.apply(expression, process);
	}
	
	public static void main(String[] args) {
		CommonInterpreter interpreter = new CommonInterpreter(map(parse("Hurrah"), parse("awesome")));
		RewritingProcess process = new DefaultRewritingProcess(null);
		process = process.put(new Categorical("Population", 5, arrayList(parse("tom")))); // two pitfalls: immutable process and need for arrayList rather than just list
		process = process.put(new Categorical("Numbers", 3, arrayList(parse("1"), parse("2"), parse("3"))));
		Expression expression = parse("false and there exists X in Numbers : X = 3 and X + 1 = 1 + X");
//		Expression expression = parse("if for all X in Population : (there exists Y in Population : Y != X) then Hurrah else not Hurrah");
		Expression result = interpreter.apply(expression, process);
		System.out.println("result: " + result);
	}
}