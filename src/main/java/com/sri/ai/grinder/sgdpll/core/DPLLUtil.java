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
package com.sri.ai.grinder.sgdpll.core;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static java.lang.Integer.parseInt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;

/**
 * Implements utility methods to be used by <code>SGDPLL(T)</code> and associated classes.
 *   
 * @author braz
 *
 */
@Beta
public class DPLLUtil {

	public static RewritingProcess makeProcess(Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate) {
		RewritingProcess result = extendProcessWith(mapFromSymbolNameToTypeName, additionalTypes, mapFromCategoricalTypeNameToSizeString, isUniquelyNamedConstantPredicate, new DefaultRewritingProcess());			
		result.setIsUniquelyNamedConstantPredicate(isUniquelyNamedConstantPredicate);
		return result;
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param additionalTypes
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param process
	 * @return
	 */
	public static RewritingProcess extendProcessWith(Map<String, String> mapFromSymbolNameToTypeName, Collection<Type> additionalTypes, Map<String, String> mapFromCategoricalTypeNameToSizeString, Predicate<Expression> isUniquelyNamedConstantPredicate, RewritingProcess process) {
		Collection<Type> allTypes =
				getCategoricalTypes(
						mapFromSymbolNameToTypeName,
						mapFromCategoricalTypeNameToSizeString,
						isUniquelyNamedConstantPredicate,
						process);
		allTypes.addAll(additionalTypes);
		
		return extendProcessWith(mapFromSymbolNameToTypeName, allTypes, process);
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param types
	 * @param process
	 * @return
	 */
	public static RewritingProcess extendProcessWith(Map<String, String> mapFromSymbolNameToTypeName, Collection<? extends Type> types, RewritingProcess process) {
		List<Expression> symbolDeclarations = new ArrayList<>();
		for (Map.Entry<String, String> variableNameAndTypeName : mapFromSymbolNameToTypeName.entrySet()) {
			String symbolName = variableNameAndTypeName.getKey();
			String typeName   = variableNameAndTypeName.getValue();
			
			symbolDeclarations.add(parse(symbolName + " in " + typeName));
		}
		process = GrinderUtil.extendContextualSymbolsWithIndexExpressions(symbolDeclarations, process);
					
		for (Type type : types) {
			process = process.newRewritingProcessWith(type);
			process.putGlobalObject(parse("|" + type.getName() + "|"), type.cardinality());
		}
		
		return process;
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param isUniquelyNamedConstantPredicate
	 * @param process
	 * @return
	 */
	public static Collection<Type> getCategoricalTypes(
			Map<String, String> mapFromSymbolNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			RewritingProcess process) {
		
		Collection<Type> categoricalTypes = new LinkedList<Type>();
		for (Map.Entry<String, String> typeNameAndSizeString : mapFromCategoricalTypeNameToSizeString.entrySet()) {
			String typeExpressionString = typeNameAndSizeString.getKey();
			String sizeString = typeNameAndSizeString.getValue();
			
			// check if already present and, if not, make it
			Categorical type = (Categorical) process.getType(typeExpressionString);
			if (type == null) {
				if (typeExpressionString.equals("Boolean")) {
					type = BOOLEAN_TYPE;
				}
				else {
					ArrayList<Expression> knownConstants = 
							getKnownUniquelyNamedConstaintsOf(
									typeExpressionString,
									mapFromSymbolNameToTypeName,
									isUniquelyNamedConstantPredicate,
									process);
					type = 
							new Categorical(
									typeExpressionString,
									parseInt(sizeString),
									knownConstants);
				}
			}
			categoricalTypes.add(type);
		}
		return categoricalTypes;
	}

	/**
	 * @param typeName
	 * @param mapFromSymbolNameToTypeName
	 * @param process
	 * @return
	 */
	public static ArrayList<Expression> getKnownUniquelyNamedConstaintsOf(String typeName, Map<String, String> mapFromSymbolNameToTypeName, Predicate<Expression> isUniquelyNamedConstantPredicate, RewritingProcess process) {
		ArrayList<Expression> knownConstants = new ArrayList<Expression>();
		for (Map.Entry<String, String> symbolNameAndTypeName : mapFromSymbolNameToTypeName.entrySet()) {
			if (symbolNameAndTypeName.getValue().equals(typeName)) {
				Expression symbol = makeSymbol(symbolNameAndTypeName.getKey());
				if (isUniquelyNamedConstantPredicate.apply(symbol)) {
					knownConstants.add(symbol);
				}
			}
		}
		return knownConstants;
	}
}