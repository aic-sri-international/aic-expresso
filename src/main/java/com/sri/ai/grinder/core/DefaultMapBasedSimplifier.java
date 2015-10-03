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
package com.sri.ai.grinder.core;

import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.putAll;
import static com.sri.ai.util.Util.sameInstancesInSameIterableOrder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A basic {@link MapBasedSimplifier} receiving its elementary simplifiers at construction time.
 * 
 * @author braz
 *
 */
@Beta
public class DefaultMapBasedSimplifier implements MapBasedSimplifier {
	
	protected Map<String, Simplifier> functionApplicationSimplifiers;
	protected Map<String, Simplifier> syntacticFormTypeSimplifiers;

	public DefaultMapBasedSimplifier(
			Map<String, Simplifier> functionApplicationSimplifiers,
			Map<String, Simplifier> syntacticFormTypeSimplifiers) {
		
		super();
		this.functionApplicationSimplifiers = functionApplicationSimplifiers;
		this.syntacticFormTypeSimplifiers = syntacticFormTypeSimplifiers;
	}

	@Override
	public Map<String, Simplifier> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, Simplifier> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	public void setFunctionApplicationSimplifiers(Map<String, Simplifier> functionApplicationSimplifiers) {
		this.functionApplicationSimplifiers = functionApplicationSimplifiers;
	}

	public void setSyntacticFormTypeSimplifiers(Map<String, Simplifier> syntacticFormTypeSimplifiers) {
		this.syntacticFormTypeSimplifiers = syntacticFormTypeSimplifiers;
	}

	/**
	 * Default implementation that simplifies an expression by exhaustively simplifying its top expression with
	 * the simplifiers provided by {@link #makeFunctionApplicationSimplifiers()} and {@link #makeSyntacticFormTypeSimplifiers()},
	 * then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
		return DefaultMapBasedSimplifier.simplify(expression, getFunctionApplicationSimplifiers(), getSyntacticFormTypeSimplifiers(), process);
	}

	/**
	 * Simplifies the top expression of an equality-logic-with-quantifiers formula until it cannot be simplified anymore.
	 * Always returns either a symbol or a function application (quantified formulas have their top quantifiers eliminated).
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormTypeSimplifiers
	 * @param process
	 * @return
	 */
	public static Expression topSimplifyExhaustively(Expression expression, Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormTypeSimplifiers, RewritingProcess process) {
		
		Expression previous;
		do {
			expression = topSimplifyOnce(previous = expression, functionApplicationSimplifiers, syntacticFormTypeSimplifiers, process);
		} while (expression != previous);
		
		return expression;
	}

	public static Expression topSimplifyOnce(Expression expression, Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormTypeSimplifiers, RewritingProcess process) {
		Simplifier simplifier;
		if (expression.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			simplifier = functionApplicationSimplifiers.get(expression.getFunctor().getValue());
		}
		else {
			simplifier = syntacticFormTypeSimplifiers.get(expression.getSyntacticFormType());
		}
		
		if (simplifier != null) {
			expression = simplifier.apply(expression, process);
		}
		
		return expression;
	}

	public static Simplifier makeTopExhaustiveSimplifier(Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormSimplifiers) {
		Simplifier
			topExhaustivelySimplifier =
			(e, p) -> topSimplifyExhaustively(e, functionApplicationSimplifiers, syntacticFormSimplifiers, p);
		return topExhaustivelySimplifier;
	}

	/**
	 * Simplifies an expression based on two maps of simplifiers.
	 * The first map of simplifiers is a map from functor values (Strings) to a binary function taking a function application of that functor and a rewriting process,
	 * and performing a simplification on it (or returning the same instance).
	 * The second map of simplifiers is a map from syntactic type forms (Strings) to a binary function taking an expression of that type and a rewriting process,
	 * and performing a simplification on it (or returning the same instance).
	 * These two maps are then used to create a top exhaustive simplifier
	 * (made with {@link #makeTopExhaustiveSimplifier(Map, Map)}) for use with {@link DPLLUtil#simplify(Expression, BinaryFunction, RewritingProcess).
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormSimplifiers
	 * @param process
	 * @return
	 */
	public static Expression simplify(Expression expression, Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormSimplifiers, RewritingProcess process) {
		Simplifier topExhaustiveSimplifier = makeTopExhaustiveSimplifier(functionApplicationSimplifiers, syntacticFormSimplifiers);
		Expression result = simplify(expression, topExhaustiveSimplifier, process);
		return result;
	}

	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with given top simplifier, then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	public static Expression simplify(
			Expression expression,
			Simplifier topSimplifier,
			RewritingProcess process) {
		
		Expression result = expression;
		result = topSimplifier.apply(result, process);
		if (result.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			List<Expression> originalArguments = result.getArguments();
			ArrayList<Expression> simplifiedArguments =
					mapIntoArrayList(originalArguments, e -> simplify(e, topSimplifier, process));
			if ( ! sameInstancesInSameIterableOrder(originalArguments, simplifiedArguments)) { // this check speeds cardinality algorithm by about 25%; it is also required for correctness wrt not returning a new instance that is equal to the input.
				result = Expressions.apply(result.getFunctor(), simplifiedArguments);
			}
			result = topSimplifier.apply(result, process);
		}
	
		return result;
	}

	/**
	 * Simplify an expression given maps of function application and syntactic form type simplifiers,
	 * and an extra simplifier for a given syntactic form type.
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormTypeSimplifiers
	 * @param process
	 * @param additionalSyntacticFormTypesAndSimplifiers additional syntactic form types and corresponding simplifiers
	 * @return
	 */
	public static Expression simplifyWithExtraSyntacticFormTypeSimplifiers(
			Expression expression,
			Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormTypeSimplifiers,
			RewritingProcess process,
			Object... additionalSyntacticFormTypesAndSimplifiers) {
		
		Map<String, Simplifier>
		mySyntacticFormTypeSimplifiers = new StackedHashMap<String, Simplifier>(syntacticFormTypeSimplifiers);
		
		putAll(mySyntacticFormTypeSimplifiers, additionalSyntacticFormTypesAndSimplifiers);
		
		Expression result = simplify(expression, functionApplicationSimplifiers, mySyntacticFormTypeSimplifiers, process);
		return result;
	}
}