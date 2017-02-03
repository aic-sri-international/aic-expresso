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

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * <pre>
 * {{ (on I1) H1 : C1 }} intersection {{ (on I2) H2 : C2 }}  
 * ---> 
 * {{ (on I1) H1 : C1 and evaluate(there exists I2 : C2 and H2 = H1) }} 
 * </pre>
 *  
 * @author oreilly
 *
 */
public class IntersectionIntensionalSetsSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (expression.hasFunctor(FunctorConstants.INTERSECTION)) {
			List<Expression> intensionalMultiSetArgs    = new ArrayList<>();
			List<Expression> nonIntensionalMultiSetArgs = new ArrayList<>();
			for (Expression arg : expression.getArguments()) {
				if (Sets.isIntensionalMultiSet(arg)) {
					intensionalMultiSetArgs.add(arg);
				}
				else {
					nonIntensionalMultiSetArgs.add(arg);
				}
			}
			if (intensionalMultiSetArgs.size() > 1) {
				boolean resultIsEmptySet = false;
				IntensionalSet intersectedMultiSet = (IntensionalSet) intensionalMultiSetArgs.get(0);
				for (int i = 1; i < intensionalMultiSetArgs.size() && !resultIsEmptySet; i++) {
					IntensionalSet otherMultiSet = standardizeApartIntensionalSets((IntensionalSet) intensionalMultiSetArgs.get(i), intersectedMultiSet, context);
					// {{ (on I1) H1 : C1 }} intersection {{ (on I2) H2 : C2 }}
					// ---->
					// {{ (on I1) H1 : C1 and evaluate(there exists I2 : C2 and H2 = H1) }}				
					IndexExpressionsSet i1 = intersectedMultiSet.getIndexExpressions();
					IndexExpressionsSet i2 = otherMultiSet.getIndexExpressions();
					Expression h1 = intersectedMultiSet.getHead();
					Expression h2 = otherMultiSet.getHead();
					Expression c1 = intersectedMultiSet.getCondition();
					Expression c2 = otherMultiSet.getCondition();
					
					Expression thereExists          = ThereExists.make(i2, And.make(c2, Equality.make(h2, h1)));
					Context    i1ExtendedContext    = (Context) GrinderUtil.extendRegistryWithIndexExpressions(i1, context);
					Expression thereExistsEvaluated = context.getTheory().evaluate(thereExists, i1ExtendedContext);
					if (thereExistsEvaluated.equals(false)) {
						// They don't intersect, which means you have an empty
						// set in the intersection, which means the whole thing
						// results in the empty set.
						resultIsEmptySet = true;
					}
					else if (!thereExistsEvaluated.equals(true)) {
						// If we have a condition, other than false and true
						// we will want to extend the current result by the condition
						Expression extendedCondition = And.make(c1, thereExistsEvaluated);
						intersectedMultiSet = (IntensionalSet) IntensionalSet.intensionalMultiSet(i1, h1, extendedCondition);					
						// Ensure we don't have a false condition.
						Expression simplifiedIntersectedMultiSet = context.getTheory().evaluate(intersectedMultiSet, context);
						if (Sets.isEmptySet(simplifiedIntersectedMultiSet)) {
							resultIsEmptySet = true;
						}
					}
				}
				if (resultIsEmptySet) {
					result = Sets.EMPTY_SET;
				}
				else if (nonIntensionalMultiSetArgs.size() > 0) {
					List<Expression> intersectedArgs = new ArrayList<>();
					intersectedArgs.add(intersectedMultiSet);
					intersectedArgs.addAll(nonIntensionalMultiSetArgs);
					result = Sets.makeIntersection(intersectedArgs.toArray(new Expression[intersectedArgs.size()]));
				}
				else {
					result = intersectedMultiSet;
				}
			}
		}
		
		return result;
	}
	
	@SuppressWarnings("deprecation")
	public static IntensionalSet standardizeApartIntensionalSets(IntensionalSet intensionalSet, IntensionalSet fromOtherIntensionalSet, Context context) {
		IntensionalSet result = intensionalSet;
		
		IndexExpressionsSet intensionalSetIndexes     = intensionalSet.getIndexExpressions();
		IndexExpressionsSet fromOtherIntensionalSetIn = fromOtherIntensionalSet.getIndexExpressions();
		List<Expression> overlappingIndexNames = new ArrayList<>();
		for (Expression intensionalSetIndex : IndexExpressions.getIndices(intensionalSetIndexes)) {
			if (IndexExpressions.indexExpressionsContainIndex(fromOtherIntensionalSetIn, intensionalSetIndex)) {
				overlappingIndexNames.add(intensionalSetIndex);
			}
		}
		if (overlappingIndexNames.size() > 0) {
			Expression combinedExpression = And.make(intensionalSet, fromOtherIntensionalSet);
			List<Expression> newIndexNames = new ArrayList<>();
			for (Expression overlappingIndex : overlappingIndexNames) {
				Expression newIndexName = Expressions.makeUniqueVariable(overlappingIndex.toString(), combinedExpression, context);
				newIndexNames.add(newIndexName);
			}
			SyntaxTree resultSyntaxTree = result.getSyntaxTree();
			for (int i = 0; i < newIndexNames.size(); i++) {
				Expression replaced    = overlappingIndexNames.get(i);
				Expression replacement = newIndexNames.get(i); 
				
				resultSyntaxTree = resultSyntaxTree.replaceSubTreesAllOccurrences(replaced.getSyntaxTree(), replacement.getSyntaxTree());
			}
			
			result = (IntensionalSet) Expressions.makeFromSyntaxTree(resultSyntaxTree);
		}
				
		return result;
	}
}