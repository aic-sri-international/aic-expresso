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

public class InversionSimplifier implements Simplifier {
	// TODO - 1. Add support for summations and products with more than 1 index.
	
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (isSummationIndexedByFunctionOfProducts(expression, context)) {
			if (isInversionPossible(expression, context)) {
				result = applyInversion(expression, context);
			}
		}
		
		return result;
	}
	
	private static Expression applyInversion(Expression summation, Context context) {
		Expression result; 
		
		// NOTE: at this point we know we have a summation indexed by a function 
		Expression summationArg = summation.get(0);
		IntensionalSet summationIntensionalSet  = (IntensionalSet) summationArg;
		IndexExpressionsSet indexExpressionsSet = summationIntensionalSet.getIndexExpressions();
		Expression functionIndex                = IndexExpressions.getIndices(indexExpressionsSet).get(0);
		Context intensionalSetContext           = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressionsSet, context);
		FunctionType functionType               = (FunctionType) GrinderUtil.getType(functionIndex, intensionalSetContext);

		List<Expression> products = new ArrayList<>();
		collectProducts(summationIntensionalSet.getHead(), products, context);
		
		List<Expression> summationIndexArgs = new ArrayList<>();
		for (Expression product : products) {
			Expression index = IndexExpressions.getIndices(((IntensionalSet) product.get(0)).getIndexExpressions()).get(0);
			summationIndexArgs.add(ExtensionalSet.makeSingleton(index));
		}
		
		Expression summationIndexType;
		if (summationIndexArgs.size() == 1) {
			summationIndexType =  Expressions.apply(FunctorConstants.FUNCTION_TYPE, summationIndexArgs.get(0), parse(functionType.getCodomain().getName()));
		}
		else {
			Expression domainTypes = Expressions.apply(FunctorConstants.TUPLE_TYPE, summationIndexArgs.toArray(new Object[summationIndexArgs.size()]));
			summationIndexType =  Expressions.apply(FunctorConstants.FUNCTION_TYPE, domainTypes, parse(functionType.getCodomain().getName()));
		}
		Expression summationIndex = IndexExpressions.makeIndexExpression(functionIndex, summationIndexType);
		
		IntensionalSet lastProductIntensionalSet = (IntensionalSet) products.get(products.size()-1).get(0);
		
		Expression innerSummation = IntensionalSet.intensionalMultiSet(new ExtensionalIndexExpressionsSet(summationIndex), 
				lastProductIntensionalSet.getHead(), summationIntensionalSet.getCondition());
		
		result = Expressions.apply(FunctorConstants.SUM, innerSummation);
		
		for (int i = products.size() -1; i >= 0; i--) {
			lastProductIntensionalSet = (IntensionalSet) products.get(i).get(0);
			Expression product = IntensionalSet.intensionalMultiSet(lastProductIntensionalSet.getIndexExpressions(), 
					result, lastProductIntensionalSet.getCondition());
			
			result = Expressions.apply(FunctorConstants.PRODUCT, product);
		}
				
		return result;
	}
	
	private static void collectProducts(Expression expression, List<Expression> products, Context context) {
		if (isProductOf(expression, context)) {
			products.add(expression);
			Expression possibleInnerProduct = ((IntensionalSet)expression.get(0)).getHead();
			collectProducts(possibleInnerProduct, products, context);
		}
	}
	
	private static boolean isSummationIndexedByFunctionOfProducts(Expression expression, Context context) {
		boolean result = false;
		
		if (expression.hasFunctor(FunctorConstants.SUM) && expression.numberOfArguments() == 1) {
			Expression summationArg = expression.get(0);
			if (Sets.isIntensionalSet(summationArg)) {
				IntensionalSet intensionalSet           = (IntensionalSet) summationArg;
				IndexExpressionsSet indexExpressionsSet = intensionalSet.getIndexExpressions();
				List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);			
				if (indices.size() == 1) {
					Expression intensionalSetIndex = indices.get(0);
					Context intensionalSetContext  = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressionsSet, context);
					Type indexType                 = GrinderUtil.getType(intensionalSetIndex, intensionalSetContext);
					if (indexType instanceof FunctionType) {
						// A summation indexed by a function of at least 1 product
						result = isProductOf(intensionalSet.getHead(), intensionalSetContext);
					}
				}				
			}
			
		}
		
		return result;
	}
	
	private static boolean isProductOf(Expression expression, Context intensionalSetConext) {
		boolean result = false;
		if (expression.hasFunctor(FunctorConstants.PRODUCT) && expression.numberOfArguments() == 1) {
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
	
	private static boolean isInversionPossible(Expression expression, Context context) {
		boolean result = true;
// TODO - actually determine if we can perform inversion.		
		return result;
	}
}
