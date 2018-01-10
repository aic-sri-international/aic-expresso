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
package com.sri.ai.grinder.library.number;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.grinder.library.FunctorConstants.MAX;

import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.CommutativeAssociativeWithOperationOnJavaConstantsOnly;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.util.Util;

/**
 * @author braz
 *
 */
@Beta
public class Max extends CommutativeAssociativeWithOperationOnJavaConstantsOnly implements Simplifier {

	// While we extend CommutativeAssociativeWithOperationOnJavaConstantsOnly,
	// we will still to deal with non-Java constants INFINITY and MINUS_INFINITY,
	// so we override a couple of their methods.
	
	private final static Predicate<Expression> isOperableArgumentPredicate =
			o -> 
	o.equals(Expressions.INFINITY) ||
	o.equals(Expressions.MINUS_INFINITY) ||
	(o instanceof Expression &&
			((Expression)o).getSyntacticFormType().equals("Symbol") && 
			((Expression)o).getValue() instanceof Number);

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		// We need to override this method because INFINITY and MINUS_INFINITY
		// are not Java constants.
		Expression result;
		if ( ! isExtensional(expression)) {
			result = expression;
		}
		else {
			// takes care of infinity arguments before deferring to super method
			if (expression.getArguments().contains(INFINITY)) {
				result = INFINITY;
			}
			else {
				// remove MINUS_INFINITY if any and defer to super method
				List<Expression> argumentsWithoutMinusInfinity =
						Util.removeNonDestructively(expression.getArguments(), MINUS_INFINITY);
				Expression expressionWithoutMinusInfinity =
						expression.getArguments() == argumentsWithoutMinusInfinity?
								expression : Expressions.apply(expression.getFunctor(), argumentsWithoutMinusInfinity);
				result = super.apply(expressionWithoutMinusInfinity, context);
			}
		}
		return result;
	}

	@Override
	public Object getFunctor() {
		return MAX;
	}
	
	@Override
	protected Expression getNeutralElement() {
		return MINUS_INFINITY;
	}
	
	@Override
	protected Expression getAbsorbingElement() {
		return INFINITY;
	}
	
	@Override
	protected boolean isIdempotent() {
		return true;
	}

	@Override
	protected Predicate<Expression> getIsOperableArgumentSyntaxTreePredicate() {
		return isOperableArgumentPredicate;
	}

	/**
	 * Overridden method to deal with special case of an empty set of arguments.
	 */
	@Override
	public Expression operationOnOperableArguments(LinkedList<Expression> operableArguments) {
		// We need to override this method because MINUS_INFINITY
		// is not a Java constant.
		if (operableArguments.isEmpty()) {
			return MINUS_INFINITY;
		}
		return super.operationOnOperableArguments(operableArguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected Object operationOnOperableValues(List listOfConstants) {
		return Util.maxArbitraryPrecision(listOfConstants);
	}

	/**
	 * Makes a max expression, automatically accounting for neutral element occurrences.
	 */
	public static Expression make(List<Expression> arguments) {
		return CommutativeAssociative.make(MAX, arguments, MINUS_INFINITY, true);
	}
}
