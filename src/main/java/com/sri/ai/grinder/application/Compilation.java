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
package com.sri.ai.grinder.application;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.core.solver.DefaultMultiQuantifierEliminator;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.group.Max;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.util.Util;

/**
 * An example on how to use SGDPLL(T) to compile expression in a given theory to decision tree representations.
 * @author braz
 *
 */
public class Compilation {

	/**
	 * Compiles an expression to a normalized (decision-tree-like) expression.
	 * @param inputExpression
	 * @param mapFromVariableNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param additionalTypes
	 * @param solverListener if not null, invoked on solver used for compilation, before and after compilation starts; returned solver on 'before' invocation is used (it may be the same one used as argument, of course).
	 * @return
	 */
	public static Expression compile(Expression inputExpression, Theory theory, Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromUniquelyNamedConstantToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Function<MultiQuantifierEliminator, MultiQuantifierEliminator> solverListener) {
		AssociativeCommutativeGroup group = new Max(); // the group actually does not matter, because we are not going to have any indices.
		
		// The solver for the parameters above.
		MultiQuantifierEliminator solver = new DefaultMultiQuantifierEliminator();
		if (solverListener != null) {
			solver = solverListener.apply(solver);
		}
		
		// We use the Prolog convention of small-letter initials for constants, but we need an exception for the random variables.
		Predicate<Expression> isPrologConstant = new PrologConstantPredicate();
		Predicate<Expression> isUniquelyNamedConstantPredicate = e -> isPrologConstant.apply(e) && ! mapFromVariableNameToTypeName.containsKey(e.toString());
		
		Map<String, String> mapFromSymbolNameToTypeName = new LinkedHashMap<>(mapFromVariableNameToTypeName);
		mapFromSymbolNameToTypeName.putAll(mapFromUniquelyNamedConstantToTypeName);
		
		// Solve the problem.
		 // no indices; we want to keep all variables
		List<Expression> indices = Util.list();
		Context context =
				GrinderUtil.makeContext(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate,
				theory);
		
		Expression result = solver.extendContextAndSolve(group, indices, inputExpression, context);	
		
		if (solverListener != null) {
			solverListener.apply(null);
		}
		return result;
	}

	public static Expression compile(Expression inputExpression, Theory theory, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromUniquelyNamedConstantToTypeName) {
		return compile(inputExpression, theory, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, null);
	}
}
