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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
import static com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions.getIndices;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.base.Triple.triple;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultTuple;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.base.Triple;

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
	default Context extendWithSymbolsAndTypes(String... symbolsAndTypes) {
		return (Context) Registry.super.extendWithSymbolsAndTypes(symbolsAndTypes);
	}
	
	default Context extendWith(IndexExpressionsSet indexExpressions) {
		return (Context) Registry.super.extendWith(indexExpressions);
	}
	
	default Context conjoin(Expression formula) {
		return conjoin(formula, this);
	}
	
	/**
	 * Extends context with index expressions, taking into account that new contextual variables may collide with existing ones.
	 * In this case, it renames the incoming variables to unique identifiers and replaces them in the types of remaining
	 * index expressions. It also renames the variables in a given expressions supposed to be in their scope (for example,
	 * the head and condition of an intensionally defined set).
	 * Returns the new context and the index expressions and expression in scope after the renaming.
	 * @param indexExpressions
	 * @param expressionInScope
	 * @return the new context and the index expressions and expression in scope after the renaming
	 */
	default 
	Triple<Context, ExtensionalIndexExpressionsSet, Expression> 
	extendWith(ExtensionalIndexExpressionsSet indexExpressions, Expression expressionInScope) {
		Triple<Context, ExtensionalIndexExpressionsSet, Expression> result;
		if (thereExists(getIndices(indexExpressions), index -> this.getSymbols().contains(index))) {
			// the objects to be returned in the triple:
			Context newContext = this;
			ArrayList<Expression> newIndexExpressionsList = new ArrayList<>(indexExpressions.getList());
			Expression newExpressionInScope = expressionInScope;
			
			// Collects all existing symbols to be able to create unique symbols
			Set<Expression> alreadyDefined = Util.set();
			alreadyDefined.addAll(this.getSymbols());
			alreadyDefined.addAll(Expressions.freeSymbols(new DefaultTuple(newIndexExpressionsList), this));
			alreadyDefined.addAll(Expressions.freeSymbols(expressionInScope, this));
			Predicate<Expression> isAlreadyDefined = e -> alreadyDefined.contains(e);
			
			for (int i = 0; i != newIndexExpressionsList.size(); i++) {
				Expression indexExpression = newIndexExpressionsList.get(i);
				Symbol index = (Symbol) indexExpression.get(0);
				Expression type = indexExpression.get(1);
				PairOf<Expression> newIndexAndNewExpressionInScope = Expressions.standardizeApart(index, isAlreadyDefined, newExpressionInScope);
				Expression newIndex  = newIndexAndNewExpressionInScope.first;
				newExpressionInScope = newIndexAndNewExpressionInScope.second;
				Expression newIndexExpression = apply(IN, newIndex, type); // type should not contain the index
				newIndexExpressionsList.set(i, newIndexExpression);
				alreadyDefined.add(newIndex);
				for (int j = i + 1; j != newIndexExpressionsList.size(); j++) {
					Expression anotherIndexExpression = newIndexExpressionsList.get(j);
					Expression anotherIndex = anotherIndexExpression.get(0);
					Expression anotherType = anotherIndexExpression.get(1);
					Expression newAnotherType = anotherType.replaceSymbol(index, newIndex, this);
					Expression newAnotherIndexExpression = apply(IN, anotherIndex, newAnotherType); // anotherIndex is a symbols and does not contain index
					newIndexExpressionsList.set(j, newAnotherIndexExpression);
				}
			}
			ExtensionalIndexExpressionsSet newIndexExpressions = new ExtensionalIndexExpressionsSet(newIndexExpressionsList);
			newContext = newContext.extendWith(newIndexExpressions);
			result = triple(newContext, newIndexExpressions, newExpressionInScope);
		}
		else {
			// no collision; usual extension and the expressions do not change.
			result = triple(extendWith(indexExpressions), indexExpressions, expressionInScope);
		}
		return result;
	}
}
