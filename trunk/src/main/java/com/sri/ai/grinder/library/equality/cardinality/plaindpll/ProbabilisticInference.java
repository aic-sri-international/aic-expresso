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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.setDifference;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.Util;

/**
 * An example on how to use SGDPLL(T) to convert table representations to decision tree representations.
 * @author braz
 *
 */
public class ProbabilisticInference {

	/**
	 * Solves a Bayesian network provided as a product of potential functions.
	 * @param bayesianNetwork an Expression representing the product of potential functions
	 * @param queryExpression an Expression representing the query
	 * @param evidence an Expression representing the evidence
	 * @param mapFromTypeNameToSizeString a map from type name strings to their size strings
	 * @param mapFromRandomVariableNameToTypeName a map from random variable name strings to their type name strings
	 * @return
	 */
	public static Expression solveBayesianNetwork(
			Expression bayesianNetwork, Expression queryExpression, Expression evidence,
			Map<String, String> mapFromTypeNameToSizeString, Map<String, String> mapFromRandomVariableNameToTypeName) {
		
		// Add a query variable equivalent to query expression; this introduces no cycles and the model remains a Bayesian network
		bayesianNetwork = Times.make(list(bayesianNetwork, parse("if query <=> " + queryExpression + " then 1 else 0")));
		mapFromRandomVariableNameToTypeName.put("query", "Boolean");
		Expression queryVariable = parse("query");
		
		Expression factorGraph = bayesianNetwork;
		
		if (evidence != null) {
			// add evidence factor
			factorGraph = Times.make(list(factorGraph, IfThenElse.make(evidence, ONE, ZERO)));
		}
		
		Collection<Expression> allVariables = Util.mapIntoList(mapFromRandomVariableNameToTypeName.keySet(), Expressions::parse);
		
		// We use the Prolog convention of small-letter initials for constants, but we need an exception for the variables.
		Predicate<Expression> isPrologConstant = new PrologConstantPredicate();
		Predicate<Expression> isUniquelyNamedConstantPredicate = e -> isPrologConstant.apply(e) && ! allVariables.contains(e);
	
		// We sum out all variables but the query
		Collection<Expression> indices = setDifference(allVariables, list(queryVariable)); 
	
		// The theory of atoms plus equality on function (relational) terms.
		Theory theory = new AtomsOnTheoryWithEquality(new EqualityTheory(new FunctionalTermTheory()));
		ProblemType problemType = new Sum(); // for marginalization
	
		// The solver for the parameters above.
		DPLLGeneralizedAndSymbolic solver = new DPLLGeneralizedAndSymbolic(theory, problemType);
		
		// Solve the problem.
		Expression unnormalizedMarginal = solver.solve(factorGraph, indices, mapFromRandomVariableNameToTypeName, mapFromTypeNameToSizeString, isUniquelyNamedConstantPredicate);
		
		Expression marginal;
		if (evidence == null) {
			marginal = unnormalizedMarginal; // factorGraph was a Bayesian network with no evidence, so marginal is equal to unnormalized marginal.
		}
		else {
			// We now marginals on all variables. Since we have the marginal on all but the query, we simply take that and marginalize on the query alone.
			Expression normalizationConstant = solver.solve(unnormalizedMarginal, list(queryVariable), mapFromRandomVariableNameToTypeName, mapFromTypeNameToSizeString, isUniquelyNamedConstantPredicate);
			System.out.println("Normalization constant (same as evidence probability P(" + evidence + ") ) is " + normalizationConstant);
	
			marginal = Division.make(unnormalizedMarginal, normalizationConstant);
			// now we use the algorithm again for simplifying the above division; this is a lazy way of doing this, as it performs search again -- we could instead write an ad hoc function to divide all numerical constants by the normalization constant.
			marginal = solver.solve(marginal, list(), mapFromRandomVariableNameToTypeName, mapFromTypeNameToSizeString, isUniquelyNamedConstantPredicate);
		}
		
		marginal = marginal.replaceAllOccurrences(queryVariable, queryExpression, new DefaultRewritingProcess(null));
		
		return marginal;
	}

	/**
	 * An example.
	 * @param args
	 */
	public static void main(String[] args) {

		// The definitions of types
		Map<String, String> mapFromTypeNameToSizeString = Util.map(
				"Folks", "10",
				"Boolean", "2");

		// The definitions of variables, types, and type sizes
		Map<String, String> mapFromVariableNameToTypeName = Util.map(
				"earthquake", "Boolean",
				"burglar", "Folks", // a multi-value random variable
				"alarm", "Boolean"
				);
		
		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		Expression bayesianNetwork = parse("" + 
				"(if earthquake then 0.01 else 0.99) * " +
				"(if burglar = bob then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglar = bob or burglar = tom or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				"");

//		Expression evidence = null; // null indicates no evidence
//		Expression evidence = parse("alarm");
//		Expression evidence = parse("not alarm"); // can be any boolean expression
		Expression evidence = parse("(alarm or not alarm) and (burglar = tom or burglar != tom)"); // tautology has same effect as no evidence
		
		Expression queryExpression = parse("burglar = bob");
//		Expression queryExpression = parse("burglar = tom");
//		Expression queryExpression = parse("earthquake");
//		Expression queryExpression = parse("earthquake and burglar = bob");
		
		Expression marginal = solveBayesianNetwork(bayesianNetwork, queryExpression, evidence, mapFromTypeNameToSizeString, mapFromVariableNameToTypeName);

		if (evidence == null) {
			System.out.println("Marginal probability P(" + queryExpression + ") is: " + marginal);
		}
		else {
			System.out.println("Query posterior probability P(" + queryExpression + " | " + evidence + ") is: " + marginal);
		}
	}
}
