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
package com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.util.base.Pair;

/**
 * A simplifier that will rewrite tuple-valued free variables:<br>
 * 
 * <pre>
 * sum( {{ (on X in 1..10) if N = (2, X) then 2 else 3 }} )
 * where N is in 1..10 x 1..10.
 * ---->
 * sum( {{ (on X in 1..10) if (N1, N2) = (2, X) then 2 else 3 }} ) 
 * 
 * Where the rewriter replaces tuple-valued variable N by its components 
 * and *saves* the association N = (N1, N2). Once the full expression
 * has been evaluated, i.e.:
 * 
 * if N1 = 2 then 29 else 30
 * 
 * Then rewriter uses the saved association N = (N1, N2) to translate back:
 * if get(N, 1) = 2 then 29 else 30
 *  
 * </pre>
 * 
 * @author oreilly
 */
public class TupleValuedFreeVariablesSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		// First see if we have any free variables.
		Map<Expression, Expression> freeVariablesAndTypes = Expressions.freeVariablesAndTypes(expression, context);
		if (freeVariablesAndTypes.size() > 0) {
			// Retrieve those that are tuples
			Map<Expression, TupleType> freeVariablesOfTupleType = 
					freeVariablesAndTypes.entrySet().stream()
						.filter(entry -> TupleType.isTupleType(entry.getValue()))
						.collect(Collectors.toMap(e -> e.getKey(), e -> (TupleType) GrinderUtil.fromTypeExpressionToItsIntrinsicMeaning(e.getValue(), context)));			
			if (freeVariablesOfTupleType.size() > 0) {
				final Map<Expression, List<Pair<Expression, Integer>>> freeVariableComponentsMap = constructComponentMap(freeVariablesOfTupleType, expression, context);

				// Replace the free tuple variables with their componentised forms
				// e.g. N --> (N1, N2)
				Expression componentisedExpression = expression.replaceAllOccurrences(expr -> {
					Expression replacement = expr;
					List<Pair<Expression, Integer>> replacementComponents = freeVariableComponentsMap.get(expr);
					if (replacementComponents != null) {
						replacement = constructComponentTuple(replacementComponents);
					}				
					return replacement;
				}, context);
				
				// Evaluate the expression with the un-componentized free tuple variables, within an extended
				// context that knows about the newly componentized variables
				Context contextExtendedWithComponentVariables = extendContextWithComponentVariables(context, freeVariablesOfTupleType, freeVariableComponentsMap);
				Expression evaluatedResult = context.getTheory().evaluate(componentisedExpression, contextExtendedWithComponentVariables);
				
			
				// Translate back the free variable components
				// e.g: 
				// if N1 = 2 then 29 else 30
				// ---->
				// if get(N, 1) = 2 then 29 else 30
				final Map<Expression, Pair<Expression, Integer>> componentToFreeVariableMap = createReverseLookupMap(freeVariableComponentsMap);
				result = evaluatedResult.replaceAllOccurrences(expr -> {
					Expression replacement = expr;
					
					Pair<Expression, Integer> correspondingFreeVariableWithIndex = componentToFreeVariableMap.get(expr);
					if (correspondingFreeVariableWithIndex != null) {
						replacement = Expressions.apply(FunctorConstants.GET, correspondingFreeVariableWithIndex.first, correspondingFreeVariableWithIndex.second);
					}
					
					return replacement;
				}, contextExtendedWithComponentVariables);
			}
		}
		
		return result;
	}
	
	private static Map<Expression, List<Pair<Expression, Integer>>> constructComponentMap(Map<Expression, TupleType> freeVariablesOfTupleType, Expression expression, Context context) {
		 Map<Expression, List<Pair<Expression, Integer>>> result = new LinkedHashMap<>();
		 
		 for (Map.Entry<Expression, TupleType> freeVariableOfTupleType : freeVariablesOfTupleType.entrySet()) {
			 List<Pair<Expression, Integer>> components = new ArrayList<>();
			 int tupleArity = freeVariableOfTupleType.getValue().getArity();
			 for (int i = 1; i <= tupleArity; i++) {
				 String proposedComponentVariableName = freeVariableOfTupleType.toString()+i;
				 Expression componentVariable = Expressions.makeUniqueVariable(proposedComponentVariableName, expression, context);
				 components.add(new Pair<>(componentVariable, i));
			 }
			 result.put(freeVariableOfTupleType.getKey(), components);
		 }
		 
		 return result;
	}
	
	private static Expression constructComponentTuple(List<Pair<Expression, Integer>> replacementComponents) {
		List<Expression> elements = replacementComponents.stream().map(pair -> pair.first).collect(Collectors.toList());
		Expression result = Expressions.makeTuple(elements);
		return result;
	}
	
	private static Context extendContextWithComponentVariables(Context context, 
			Map<Expression, TupleType> freeVariablesOfTupleType, 
			Map<Expression, List<Pair<Expression, Integer>>> freeVariableComponentsMap) {
		Map<String, String> mapFromSymbolNameToTypeName = new LinkedHashMap<>();
		Set<Type> componentTypes = new LinkedHashSet<>();
		
		for (Map.Entry<Expression, TupleType> freeVariableOfTupleType : freeVariablesOfTupleType.entrySet()) {
			Expression freeVariable          = freeVariableOfTupleType.getKey();
			TupleType  freeVariableTupleType = freeVariableOfTupleType.getValue();
			componentTypes.addAll(freeVariableTupleType.getElementTypes());
			List<Pair<Expression, Integer>> components = freeVariableComponentsMap.get(freeVariable);
			for (Pair<Expression, Integer> freeVariableComponent : components) {
				Expression freeVariableComponentVar = freeVariableComponent.first;
				Type freeVariableComponentType = freeVariableTupleType.getElementTypes().get(freeVariableComponent.second -1);
				mapFromSymbolNameToTypeName.put(freeVariableComponentVar.toString(), freeVariableComponentType.getName());
			}
		}
		
		Context result = (Context) GrinderUtil.extendRegistryWith(mapFromSymbolNameToTypeName, componentTypes, context);
		return result;
	}
	
	private static Map<Expression, Pair<Expression, Integer>> createReverseLookupMap(Map<Expression, List<Pair<Expression, Integer>>> freeVariableComponentsMap) {
		Map<Expression, Pair<Expression, Integer>> result = new LinkedHashMap<>();
		
		for (Map.Entry<Expression, List<Pair<Expression, Integer>>> freeVariableComponents : freeVariableComponentsMap.entrySet()) {
			Expression freeVariable = freeVariableComponents.getKey();
			for (Pair<Expression, Integer> component : freeVariableComponents.getValue()) {
				Expression componentVariable = component.first;
				Integer    componentIndex    = component.second;
				result.put(componentVariable, new Pair<>(freeVariable, componentIndex));
			}
		}
		
		return result;
	}
}