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

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.LinkedHashMap;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.MultiVariableContextWithCheckedProperty;

/**
 * An implementation of {@link Context} extending {@link AbstractTrivialContext} to be a {@link TRUE} constraint.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public class TrueContext extends AbstractTrivialContext {
	
	private static final long serialVersionUID = 1L;

	public TrueContext(
			Theory theory,
			Map<Expression, Expression> symbolsAndTypes,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Map<Object, Object> globalObjects) {

		super(theory,
				symbolsAndTypes,
				isUniquelyNamedConstantPredicate,
				globalObjects);
	}

	public TrueContext() {
		this(
				null,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
				new PrologConstantPredicate(), 
				new LinkedHashMap<Object, Object>()); // globalObjects
	}

	public TrueContext(Theory theory) {
		this(
				theory,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
				new PrologConstantPredicate(),
				new LinkedHashMap<Object, Object>()); // globalObjects
	}
	
	/**
	 * Creates a {@link TrueContext} containing the basic information
	 * from another context.
	 * The basic information are the theory, symbols and types, is unique constant predicate,
	 * and global objects.
	 * Uses {@link #TrueContext(Theory, Map, Predicate, Map)}.
	 * @param another
	 */
	public TrueContext(Context another) {
		this(
				another.getTheory(), 
				another.getSymbolsAndTypes(), 
				another.getIsUniquelyNamedConstantPredicate(), 
				another.getGlobalObjects());
	}

	@Override
	public TrueContext clone() {
		TrueContext result = (TrueContext) super.clone();
		return result;
	}

	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		return explanationBlock("TrueContext.conjoinWithLiteral of ", this, " with literal ", literal, code(() -> {
			
			Context result = MultiVariableContextWithCheckedProperty.conjoinTrueContextWithLiteralAsCompleteMultiVariableContext(literal, getTheory(), this);
			return result;
			
		}), "Result is ", RESULT);
	}

	@Override
	public boolean isContradiction() {
		return false;
	}

	@Override
	public Context makeContradiction() {
		Context result = 
				new FalseContext(
						getTheory(),
						getSymbolsAndTypes(),
						getIsUniquelyNamedConstantPredicate(),
						getGlobalObjects());
		return result;
	}
	
	@Override
	protected Expression computeInnerExpression() {
		return TRUE;
	}

}
