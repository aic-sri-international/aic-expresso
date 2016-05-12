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
 * Neither the name of the aic-praise nor the names of its
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
package com.sri.ai.grinder.helper;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultMonomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.util.base.Pair;


/**
 * Utility methods for isolating a generalized variable.
 * 
 * // TODO - full class comment based on Rodrigo's notes. 
 * 
 * @author oreilly
 */
@Beta
public class IsolateUtil {	
	public static Expression isolate(Expression inequality, Expression variableToIsolate) {
		if (inequality.getFunctor() == null || inequality.numberOfArguments() != 2) {
			throw new IllegalArgumentException("Not an inequality: "+inequality);
		}
		
		Expression leftExpression  = inequality.get(0);
		Expression operator        = inequality.getFunctor();
		Expression rightExpression = inequality.get(1);
		
		Expression result = isolate(leftExpression, operator, rightExpression, variableToIsolate);
		
		return result;
	}
	
	public static Expression isolate(Expression leftExpression, Expression operator, Expression rightExpression, Expression variableToIsolate) {
		Polynomial leftPolynomial  = DefaultPolynomial.make(leftExpression);
		Polynomial rightPolynomial = DefaultPolynomial.make(rightExpression);
		
		Expression result = isolate(leftPolynomial, operator, rightPolynomial, variableToIsolate);
		
		return result;
	}

	public static Expression isolate(Polynomial leftPolynomial, Expression operator, Polynomial rightPolynomial, Expression variableToIsolate) {		
		assertSupportedOperator(operator);
		
		Pair<List<Monomial>, List<Monomial>> splitIntoSimilarAndDissimilarTerms = splitIntoSimilarAndDissimilarTerms(leftPolynomial, variableToIsolate);
		List<Monomial> leftSimilarTerms     = splitIntoSimilarAndDissimilarTerms.first;
		List<Monomial> leftDissimilarTerms  = splitIntoSimilarAndDissimilarTerms.second;  
		splitIntoSimilarAndDissimilarTerms  = splitIntoSimilarAndDissimilarTerms(rightPolynomial, variableToIsolate);
		List<Monomial> rightSimilarTerms    = splitIntoSimilarAndDissimilarTerms.first;
		List<Monomial> rightDissimilarTerms = splitIntoSimilarAndDissimilarTerms.second;
		
		//
		// First move the left dissimilar terms over to the right side
		for (Monomial leftDissimilarTerm : leftDissimilarTerms) {
			rightDissimilarTerms.add(DefaultMonomial.make(Times.make(Expressions.MINUS_ONE, leftDissimilarTerm)));
		}
		
		//
		// Second move the right similar terms over to the left side
		for (Monomial rightSimilarTerm : rightSimilarTerms) {
			leftSimilarTerms.add(DefaultMonomial.make(Times.make(Expressions.MINUS_ONE, rightSimilarTerm)));
		}
		
		Polynomial leftSimilarPolynomial = DefaultPolynomial.make(Plus.make(new ArrayList<>(leftSimilarTerms)));
		Monomial isolated = null;		
		// Compute Alpha
		List<Expression> alphaTerms = new ArrayList<>();
		for (Monomial similarTerm : leftSimilarPolynomial.getOrderedSummands()) {
			List<Expression> dissimilarFactors = new ArrayList<>();
			dissimilarFactors.add(Expressions.makeSymbol(similarTerm.getNumericConstantFactor()));
			for (Expression factor : similarTerm.getOrderedNonNumericFactors()) {
				if (factor.equals(variableToIsolate)) {
					if (isolated == null) {
						isolated = DefaultMonomial.make(Exponentiation.make(factor, Expressions.makeSymbol(similarTerm.getPowerOfFactor(factor))));
					}
// TODO - ensure powers are the same across terms, if not implies > 1 solution and we can't isolate.					
				}
				else {
					dissimilarFactors.add(Exponentiation.make(factor, Expressions.makeSymbol(similarTerm.getPowerOfFactor(factor))));
				}
			}
			alphaTerms.add(DefaultMonomial.make(Times.make(dissimilarFactors)));
		}
		Polynomial rightDissimilarPolynomial = DefaultPolynomial.make(Plus.make(new ArrayList<Expression>(rightDissimilarTerms)));
		Polynomial alpha = DefaultPolynomial.make(Plus.make(alphaTerms), rightDissimilarPolynomial.getVariables());
					
		Expression left, rightDissimilar;
		if (isolated == null || alpha.equals(Expressions.ZERO)) {
			left            = alpha;
			rightDissimilar = rightDissimilarPolynomial;
		}
		else {
			left            = isolated;			
			Pair<Polynomial, Polynomial> quotientAndRemainder = rightDissimilarPolynomial.divide(alpha);
			if (quotientAndRemainder.second.equals(Expressions.ZERO)) {
				rightDissimilar = quotientAndRemainder.first;
			}
			else {
				rightDissimilar = Division.simplify(Division.make(rightDissimilarPolynomial, alpha));
			}
		}
// TODO - determine if the operator needs to be flipped
		Expression result =  Expressions.apply(operator, left, rightDissimilar);
		
		return result;
	}
	
	public static Pair<List<Monomial>, List<Monomial>> splitIntoSimilarAndDissimilarTerms(Polynomial polynomial, Expression generalizedVariable) {
		Pair<List<Monomial>, List<Monomial>> result = new Pair<>(new ArrayList<>(), new ArrayList<>());
		
		for (Monomial term : polynomial.getOrderedSummands()) {
			if (term.getFactors().contains(generalizedVariable)) {
				result.first.add(term);
			}
			else {
				result.second.add(term);
			}
		}
		
		return result;
	}
	
	
	public static void assertSupportedOperator(Expression operator) {		
		if (!(operator.equals(FunctorConstants.EQUALITY) ||
			  operator.equals(FunctorConstants.DISEQUALITY) ||
			  operator.equals(FunctorConstants.LESS_THAN) ||
			  operator.equals(FunctorConstants.GREATER_THAN) ||
			  operator.equals(FunctorConstants.LESS_THAN_OR_EQUAL_TO) ||
			  operator.equals(FunctorConstants.GREATER_THAN_OR_EQUAL_TO) )) {
			throw new IllegalArgumentException("Operator "+operator+" is not supported");
		}
	}
}