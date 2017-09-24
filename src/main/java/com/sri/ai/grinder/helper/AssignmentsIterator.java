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
package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductInMapIterator;

/**
 * An iterator over assignments to expressions according to their {@link Type} as defined
 * in a {@link Registry}.
 * 
 * @author braz
 */
@Beta
public class AssignmentsIterator extends CartesianProductInMapIterator<Expression, Expression> {

	public AssignmentsIterator(Collection<Expression> variables, Registry registry) {
		super(makeMapFromVariablesToIteratorMakers(variables, registry));
	}

	public AssignmentsIterator(IndexExpressionsSet indexExpressionsSet, Registry registry) {
		super(makeMapFromVariablesToIteratorMakersFrom(indexExpressionsSet, registry));
	}

	private static Map<Expression, NullaryFunction<Iterator<Expression>>>
	makeMapFromVariablesToIteratorMakers(Collection<Expression> variables, Registry registry) {
		Map<Expression, NullaryFunction<Iterator<Expression>>> fromVariableToIteratorMaker = map();
		for (Expression variable : variables) {
			Expression typeDescription = GrinderUtil.getTypeExpression(variable, registry);
			putVariableAndIteratorMakerIn(fromVariableToIteratorMaker, variable, typeDescription, registry);
		}
		return fromVariableToIteratorMaker;
	}

	private static Map<Expression, NullaryFunction<Iterator<Expression>>>
	makeMapFromVariablesToIteratorMakersFrom(IndexExpressionsSet indexExpressionsSet, Registry registry) {
		Map<Expression, NullaryFunction<Iterator<Expression>>> fromVariableToIteratorMaker = map();
		ExtensionalIndexExpressionsSet extensionalIndexExpressionsSet;
		try {
			extensionalIndexExpressionsSet = (ExtensionalIndexExpressionsSet) indexExpressionsSet;
		}
		catch (ClassCastException e) {
			throw new Error("AssignmentsIterator defined for extensional index expressions sets only.");
		}
		for (Expression indexExpression : extensionalIndexExpressionsSet.getList()) {
			Expression variable = IndexExpressions.getIndex(indexExpression);
			Expression typeDescription = IndexExpressions.getType(indexExpression);
			if (typeDescription == null) {
				typeDescription = GrinderUtil.getTypeExpression(variable, registry);
			}
			putVariableAndIteratorMakerIn(fromVariableToIteratorMaker, variable, typeDescription, registry);
		}
		return fromVariableToIteratorMaker;
	}

	/**
	 * @param fromVariableToIteratorMaker
	 * @param variable
	 * @param typeExpression
	 * @param registry
	 * @throws Error
	 */
	private static void putVariableAndIteratorMakerIn(Map<Expression, NullaryFunction<Iterator<Expression>>> fromVariableToIteratorMaker, Expression variable, Expression typeExpression, Registry registry) throws Error {
		if (typeExpression == null) {
			throw new Error("Variable " + variable + " is not registered in registry (has no type).");
		}
		Type type = registry.getTypeFromTypeExpression(typeExpression);
		if (type == null) {
			throw new Error("Variable " + variable + " has type " + typeExpression + " but registry contains no type with this name.");
		}
		fromVariableToIteratorMaker.put(variable, () -> type.iterator());
	}
}
