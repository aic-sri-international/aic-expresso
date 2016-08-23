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
package com.sri.ai.grinder.sgdpll.simplifier.api;

import static com.sri.ai.util.Util.putAll;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll.api.Context;
import com.sri.ai.grinder.sgdpll.simplifier.core.DefaultMapBasedTopSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.RecursiveExhaustiveMapBasedSimplifier;
import com.sri.ai.util.collect.StackedHashMap;

@Beta
/** 
 * An type of {@link Simplifier} based on maps from functor or syntactic form types to
 * elementary simplifiers in the form of {@link BinaryFunction}s
 * from an {@link Expression} and {@link Context} to an {@link Expression},
 * which are applied exhaustively, in a top-down manner, to expressions to be simplified. 
 */
public interface MapBasedSimplifier extends Simplifier {

	Map<String, Simplifier> getFunctionApplicationSimplifiers();

	Map<String, Simplifier> getSyntacticFormTypeSimplifiers();

	/**
	 * Returns a top simplifier constructed from this {@link MapBasedSimplifier}'s
	 * elementary function and syntactic form type simplifiers.
	 * @return
	 */
	default TopSimplifier getTopSimplifier() {
		DefaultMapBasedTopSimplifier topSimplifier = 
				new DefaultMapBasedTopSimplifier(
						getFunctionApplicationSimplifiers(),
						getSyntacticFormTypeSimplifiers());
		return topSimplifier;
	}
	
	/**
	 * Simplify an expression given maps of function application and syntactic form type simplifiers,
	 * and an extra simplifier for a given syntactic form type.
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormTypeSimplifiers
	 * @param context
	 * @param additionalSyntacticFormTypesAndSimplifiers additional syntactic form types and corresponding simplifiers
	 * @return
	 */
	public static Expression simplifyWithExtraSyntacticFormTypeSimplifiers(
			Expression expression,
			Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormTypeSimplifiers,
			Context context,
			Object... additionalSyntacticFormTypesAndSimplifiers) {
		
		Map<String, Simplifier>
		mySyntacticFormTypeSimplifiers = new StackedHashMap<String, Simplifier>(syntacticFormTypeSimplifiers);
		
		putAll(mySyntacticFormTypeSimplifiers, additionalSyntacticFormTypesAndSimplifiers);
		
		Expression result = simplify(expression, functionApplicationSimplifiers, mySyntacticFormTypeSimplifiers, context);
		return result;
	}

	/**
	 * Simplifies an expression based on two maps of simplifiers by creating an instance of {@link RecursiveExhaustiveMapBasedSimplifier}
	 * with them and applies it to the expression.
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormSimplifiers
	 * @param context
	 * @return
	 */
	public static Expression simplify(Expression expression, Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormSimplifiers, Context context) {
		RecursiveExhaustiveMapBasedSimplifier simplifier = new RecursiveExhaustiveMapBasedSimplifier(functionApplicationSimplifiers, syntacticFormSimplifiers);
		Expression result = simplifier.apply(expression, context);
		return result;
	}
}