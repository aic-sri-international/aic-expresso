/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.util.base.Pair;

public class InversionSimplifier implements Simplifier {
	// TODO - 1. Add support for summations and products with more than 1 index.
	
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (isSummationIndexedByFunctionOfProducts(expression, context)) {
			// NOTE: at this point we know we have a summation indexed by a function
			Expression summationIndexedByFunction = expression;
			
			Pair<Expression, FunctionType> indexAndFunctionType = getIndexAndFunctionType(summationIndexedByFunction, context);
			Expression summationIndex                           = indexAndFunctionType.first;
			FunctionType summationIndexFunctionType             = indexAndFunctionType.second;
			
			List<Expression> products = new ArrayList<>();
			collectProducts(getHead(summationIndexedByFunction), products);
			if (isInversionPossible(summationIndex, summationIndexFunctionType, products, context)) {				
				result = applyInversion(summationIndexedByFunction, summationIndex, summationIndexFunctionType, products, context);
			}
		}
		
		return result;
	}
	
	private static boolean isInversionPossible(Expression functionName, FunctionType functionType, List<Expression> products, Context context) {
		boolean result = false;
		
		if (functionType.getArity() == products.size()) {
// TODO - more checks.			
			result = true;			
		}
		
		return result;
	}
	
	private static Expression applyInversion(Expression summationIndexedByFunction, Expression summationFunctionIndex, FunctionType summationFunctionType, List<Expression> products, Context context) {
		Expression result; 
		
		List<Expression> summationIndexArgs = new ArrayList<>();
		for (Expression product : products) {
			Expression index = getIndexAndType(product).first;
			summationIndexArgs.add(ExtensionalSet.makeSingleton(index));
		}
		
		// TODO - remove temporary hack which collapses the function's domain so that onlyt its co-domain is used
		// due to all the domain arguments being treated as constants.
		Expression summationIndex = IndexExpressions.makeIndexExpression(summationFunctionIndex, parse(summationFunctionType.getCodomain().getName()));
		
		Expression lastProduct = products.get(products.size()-1);
		
		Expression phi = getHead(lastProduct).replaceAllOccurrences(e -> {
			Expression r = e;
			if (e.hasFunctor(summationFunctionIndex)) {
				r = summationFunctionIndex;
			}
			return r;
		}, context);
		
		Expression innerSummation = IntensionalSet.intensionalMultiSet(
				new ExtensionalIndexExpressionsSet(summationIndex), 
				phi, getCondition(summationIndexedByFunction));
		
		result = Expressions.apply(FunctorConstants.SUM, innerSummation);
		
		for (int i = products.size() -1; i >= 0; i--) {
			lastProduct = products.get(i);
			Expression product = IntensionalSet.intensionalMultiSet(
					getIndexExpressions(lastProduct), result, getCondition(lastProduct));
			
			result = Expressions.apply(FunctorConstants.PRODUCT, product);
		}
				
		return result;
	}
	
	private static void collectProducts(Expression expression, List<Expression> products) {
		if (isFunctionOnIntensionalSetWithSingleIndex(FunctorConstants.PRODUCT, expression)) {
			products.add(expression);
			collectProducts(getHead(expression), products);
		}
	}
	
	private static boolean isSummationIndexedByFunctionOfProducts(Expression expression, Context context) {
		boolean result = false;
		
		if (isFunctionOnIntensionalSetWithSingleIndex(FunctorConstants.SUM, expression)) {
			Pair<Expression, Expression> indexAndType = getIndexAndType(expression);
			if (indexAndType.second != null && indexAndType.second.hasFunctor(FunctorConstants.FUNCTION_TYPE)) {
				// A summation indexed by a function of at least 1 product
				result = isFunctionOnIntensionalSetWithSingleIndex(FunctorConstants.PRODUCT, getHead(expression));
			}					
		}
		
		return result;
	}
	
	private static boolean isFunctionOnIntensionalSetWithSingleIndex(Object functor, Expression expression) {
		boolean result = false;
		if (expression.hasFunctor(functor) && expression.numberOfArguments() == 1) {
			Expression productArg = expression.get(0);
			if (Sets.isIntensionalSet(productArg)) {
				IntensionalSet intensionalSet           = (IntensionalSet) productArg;
				IndexExpressionsSet indexExpressionsSet = intensionalSet.getIndexExpressions();
				List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);			
				if (indices.size() == 1) {
					result = true;
				}
			}
		}
		return result;
	}
	
	private static IntensionalSet getIntensionalSet(Expression functionOnIntensionalSet) {
		IntensionalSet result = (IntensionalSet) functionOnIntensionalSet.get(0);
		return result;
	}
	
	private static IndexExpressionsSet getIndexExpressions(Expression functionOnIntensionalSet) {
		IndexExpressionsSet result = getIntensionalSet(functionOnIntensionalSet).getIndexExpressions();
		return result;
	}	
	
	private static Expression getHead(Expression functionOnIntensionalSet) {
		Expression result = getIntensionalSet(functionOnIntensionalSet).getHead();
		return result;
	}
	
	private static Expression getCondition(Expression functionOnIntensionalSet) {
		Expression result = getIntensionalSet(functionOnIntensionalSet).getCondition();
		return result;
	}
	
	private static Pair<Expression, Expression> getIndexAndType(Expression functionOnIntensionalSet) {
		IndexExpressionsSet indexExpressionsSet   = getIndexExpressions(functionOnIntensionalSet);		
		List<Expression> indexExpressionsWithType = IndexExpressions.getIndexExpressionsWithType(indexExpressionsSet);
		if (indexExpressionsWithType.size() != 1) {
			throw new UnsupportedOperationException("Currently only support singular indices");
		}	
		Pair<Expression, Expression> result = IndexExpressions.getIndexAndDomain(indexExpressionsWithType.get(0));
		return result;
	}
	
	private static Pair<Expression, FunctionType> getIndexAndFunctionType(Expression functionOnIntensionalSet, Context context) {
		IndexExpressionsSet indexExpressionsSet = getIndexExpressions(functionOnIntensionalSet);		
		List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);
		if (indices.size() != 1) {
			throw new UnsupportedOperationException("Currently only support singular indices");
		}
		Expression index              = indices.get(0);
		Context intensionalSetContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressionsSet, context);
		Type type                     = GrinderUtil.getType(index, intensionalSetContext);
		
		FunctionType functionType = null;
		if (type instanceof FunctionType) {
			functionType = (FunctionType) type;
		}
		
		Pair<Expression, FunctionType> result = new Pair<>(index, functionType);
		return result;
	}
}
