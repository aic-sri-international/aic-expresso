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
package com.sri.ai.grinder.sgdpllt.api;

import java.util.Collection;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Registry;

/**
 * A Context combines a {@link Registry} and a {@link Constraint}
 * in order to provide a context for SGDPLL(T)-related methods.
 * 
 * @author braz
 */
@Beta
public interface Context extends Registry, Constraint {

	@Override
	Context clone();
	
	@Override
	Context setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate);
	
	@Override
	Context registerAdditionalSymbolsAndTypes(Map<Expression, Expression> indicesAndTypes);

	@Override
	Context putAllGlobalObjects(Map<Object, Object> objects);
	
	@Override
	Context putGlobalObject(Object key, Object value);
	
	@Override
	Context add(Type type);

	@Override
	default Context addAll(Collection<Type> types) {
		return (Context) Registry.super.addAll(types);
	}
	
	
	// Constraint method (specializing return value to Context):
	
	@Override
	Context conjoinWithLiteral(Expression literal, Context context);
	
	@Override
	default Context conjoin(Expression formula, Context context) {
		return (Context) Constraint.super.conjoin(formula, context);
	}

	@Override
	default Context conjoinWithConjunctiveClause(Expression conjunctiveClause, Context context) {
		return (Context) Constraint.super.conjoinWithConjunctiveClause(conjunctiveClause, context);
	}
	
	@Override
	Context makeContradiction();
	
	/**
	 * Convenience for <code>this.getTheory().isLiteral(expression, this)</code>.
	 * @param expression
	 * @return
	 */
	default boolean isLiteral(Expression expression) {
		boolean result = getTheory().isLiteralOrBooleanConstant(expression, this);
		return result;
	}

	/**
	 * Extends with pairs of symbols and their respective types represented as strings.
	 * @param symbolsAndTypes
	 * @return
	 */
	@Override
	default Context extendWithSymbolsAndTypes(Expression... symbolsAndTypes) {
		return (Context) Registry.super.extendWithSymbolsAndTypes(symbolsAndTypes);
	}
	
	/**
	 * Extends with pairs of symbols and their respective types represented as strings.
	 * @param symbolsAndTypes
	 * @return
	 */
	@Override
	default Context extendWithSymbols(String... symbolsAndTypes) {
		return (Context) Registry.super.extendWithSymbols(symbolsAndTypes);
	}
	
	default Context extendWith(IndexExpressionsSet indexExpressions) {
		return (Context) Registry.super.extendWith(indexExpressions);
	}
	
	default Context conjoin(Expression formula) {
		return conjoin(formula, this);
	}
}
