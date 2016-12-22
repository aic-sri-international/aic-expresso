/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.sri.ai.expresso.api.CountingFormula;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.core.DefaultCountingFormula;
import com.sri.ai.expresso.core.DefaultLambdaExpression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * A simplifier that will rewrite quantified expressions containing tuple types 
 * in their index expressions as follows:<br>
 * 
 * <pre>
 * Quantifier on t in TupleType : Constraint Body
 * ---->
 * Quantifier var_1 in TupleType[1] ... Quantifier var_n in TupleType[n[
 *    : Body[t/(var_1,...,var_n)]
 *    
 * For example:
 * sum_t in Integer x Integer : t != (1,2) if t = (1,1) then 1 else 2
 * ---->
 * sum_t in Integer sum_y in Integer : (x,y) != (1,2) if (x,y) = (1,1) then 1 else 2
 * </pre>
 * 
 * @author oreilly
 */
public class TupleQuantifierSimplifier implements Simplifier {

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		if (expression instanceof QuantifiedExpression) {
			QuantifiedExpression quantifiedExpression = (QuantifiedExpression) expression;
			Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(quantifiedExpression);
			List<Map.Entry<Expression, Expression>> indexesOfTupleType = indexToTypeMap.entrySet().stream()
					.filter(entry -> entry.getValue() != null && TupleType.isTupleType(entry.getValue()))
					.collect(Collectors.toList());
			
			if (indexesOfTupleType.size() > 0) {
				Map<Expression, Expression> indexToTupleOfVars = createTuplesOfVarsForTupleTypes(quantifiedExpression, indexesOfTupleType);
				result = rewriteQuantifiedExpression(quantifiedExpression, indexToTypeMap, indexToTupleOfVars, context);
			}
		}
		return result;
	}

	private static Map<Expression, Expression> createTuplesOfVarsForTupleTypes(QuantifiedExpression quantifiedExpression, List<Map.Entry<Expression, Expression>> indexesOfTupleType) {
		Map<Expression, Expression> result = new HashMap<>();
		
		Set<Expression> allSubExpressions = Util.addAllToSet(new SubExpressionsDepthFirstIterator(quantifiedExpression));
		for (Map.Entry<Expression, Expression> entry : indexesOfTupleType) {
			List<Expression> tupleVars = new ArrayList<>();
			for (int i = 1; i <= entry.getValue().numberOfArguments(); i++) {
				Expression proposedVar = Expressions.makeSymbol(entry.getKey().toString()+"_"+i);
				Expression actualVar   = Expressions.primedUntilUnique(proposedVar, expr -> !allSubExpressions.contains(expr));
				tupleVars.add(actualVar);
			}
			result.put(entry.getKey(), Expressions.makeTuple(tupleVars));
		}
		
		return result;
	}
	
	private static Expression rewriteQuantifiedExpression(Expression quantifiedExpression, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {
		Expression result = quantifiedExpression;
		
		if (ForAll.isForAll(quantifiedExpression)) {
			result = rewriteForAll(quantifiedExpression, indexToTypeMap, indexToTupleOfVars, context);
		}
		else if (ThereExists.isThereExists(quantifiedExpression)) {
			result = rewriteThereExists(quantifiedExpression, indexToTypeMap, indexToTupleOfVars, context);
		}
		else if (quantifiedExpression instanceof CountingFormula) {
			result = rewriteCountingFormula((CountingFormula) quantifiedExpression, indexToTypeMap, indexToTupleOfVars, context);
		}
		else if (quantifiedExpression instanceof LambdaExpression) {
			result = rewriteLambdaExpression((LambdaExpression) quantifiedExpression, indexToTypeMap, indexToTupleOfVars, context);
		}
		else if (quantifiedExpression instanceof IntensionalSet) {
			result = rewriteIntensionalSet((IntensionalSet) quantifiedExpression, indexToTypeMap, indexToTupleOfVars, context);
		}
		else {
			throw new UnsupportedOperationException("Quantifer currently not supported : "+quantifiedExpression);
		}
		
		return result;
	}
	
	private static Expression rewriteForAll(Expression quantifiedExpression, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {
		if (indexToTypeMap.size() > 1) {
			throw new IllegalStateException("We have a Universal Quantifier with > 1 index : "+quantifiedExpression);
		}
		
		Pair<IndexExpressionsSet, Expression> updatePair = update(ForAll.getBody(quantifiedExpression), indexToTypeMap, indexToTupleOfVars, context);
		Expression result = ForAll.make(updatePair.first, updatePair.second);
		
		return result;
	}
	
	private static Expression rewriteThereExists(Expression quantifiedExpression, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {
		if (indexToTypeMap.size() > 1) {
			throw new IllegalStateException("We have an Existential Quantifier with > 1 index : "+quantifiedExpression);
		}
		
		Pair<IndexExpressionsSet, Expression> updatePair = update(ThereExists.getBody(quantifiedExpression), indexToTypeMap, indexToTupleOfVars, context);
		Expression result = ThereExists.make(updatePair.first, updatePair.second);
		
		return result;
	}
	
	private static Expression rewriteCountingFormula(CountingFormula countingFormula, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {		
		Pair<IndexExpressionsSet, Expression> updatePair = update(countingFormula.getBody(), indexToTypeMap, indexToTupleOfVars, context);
		Expression result = new DefaultCountingFormula(updatePair.first, updatePair.second);
		
		return result;
	}
	
	private static Expression rewriteLambdaExpression(LambdaExpression lambdaExpression, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {
		Pair<IndexExpressionsSet, Expression> updatePair = update(lambdaExpression.getBody(), indexToTypeMap, indexToTupleOfVars, context);
		Expression result = new DefaultLambdaExpression(updatePair.first, updatePair.second);
		
		return result;
	}
	
	private static Expression rewriteIntensionalSet(IntensionalSet intensionalSet, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {
		Expression headAndConditionPair = Expressions.makeTuple(intensionalSet.getHead(), intensionalSet.getCondition());
		
		Pair<IndexExpressionsSet, Expression> updatePair = update(headAndConditionPair, indexToTypeMap, indexToTupleOfVars, context);
		Expression updatedHead = updatePair.second.get(0);
		Expression updatedCondition = updatePair.second.get(1);
		
		Expression result;
		if (intensionalSet.isUniSet()) {
			result = IntensionalSet.intensionalUniSet(updatePair.first, updatedHead, updatedCondition);
		}
		else {
			result = IntensionalSet.intensionalMultiSet(updatePair.first, updatedHead, updatedCondition);
		}
		
		return result;
	}
	
	private static Pair<IndexExpressionsSet, Expression> update(Expression expression, Map<Expression, Expression> indexToTypeMap, Map<Expression, Expression> indexToTupleOfVars, Context context) {		
		List<Expression> indexExpressions = new ArrayList<>();
		Expression substitutedExpression = expression;		
		for (Map.Entry<Expression, Expression> entry : indexToTypeMap.entrySet()) {
			Expression entryIndex = entry.getKey();
			Expression entryType  = entry.getValue();
			Expression tupleVars   = indexToTupleOfVars.get(entryIndex);
			if (tupleVars != null) {
				substitutedExpression = substitutedExpression.replaceAllOccurrences(entryIndex, tupleVars, context);
				for (int i = 0; i < entryType.numberOfArguments(); i++) {
					indexExpressions.add(Expressions.apply(FunctorConstants.IN, tupleVars.get(i), entryType.get(i)));
				}
			}
			else {
				if (entryType == null) {
					indexExpressions.add(entryIndex);
				}
				else {
					indexExpressions.add(Expressions.apply(FunctorConstants.IN, entryIndex, entryType));
				}
			}
		}
		
		ExtensionalIndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(indexExpressions);
		
		return new Pair<>(indexExpressionsSet, substitutedExpression);
	}
}
