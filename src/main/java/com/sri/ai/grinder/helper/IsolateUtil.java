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
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultMonomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;


/**
 * Utility methods for isolating a generalized linear variable.
 * 
 * A basic procedure to isolate a variable. 
 * 
 * <pre>
 * Formally, given the application of a relational operator op1 (=, !=, <, >, <=, >=)
 * 
 * t1 + ... + t_m op1 r_1 + ... + r_n
 * 
 * and given a `linear generalized variable` V, derive a new <emph>equivalent</emph> application:
 * 
 * V op2 Term/Alpha
 * 
 * Where V does not appear on Term and Alpha, which must be normalized polynomials. 
 * Alpha may be 0 if V does not appear originally or disappears. V may disappear 
 * from the expression, for example, x = x becomes 0 = 0, also known as `true`. 
 * This still fits `Alpha*V op2 Term`, with Alpha being 0, but now `Alpha` cannot 
 * move to the right-hand side as a denominator. In these cases, we simply return 
 * `0 op2 Term`. 
 * 
 * op2 is the same as op1, if op1 is an equality (i.e. `=` or `!=`), otherwise it can be 
 * a `flipped` op1 if you multiply or divide the inequality by a negative 
 * (or swap the left and right hand sides).
 *
 * In the cases where it cannot be determined if `Alpha` is 0 or not, for example:
 * 
 * y * x = 10 
 * 
 * the resulting expression needs to test for `Alpha` being 0. With respect to the 
 * previous example we would get:
 * 
 * if y != 0 then x = 10/y else 0 = 10
 * 
 * or more generally:
 * 
 * if Alpha != 0 then V = Term/Alpha else 0 = Term
 * 
 * Similarly, if the sign of `Alpha` cannot be determined and `op1` is an inequality 
 * (i.e. `<`, `<=`, `>=`, or `>`) then the general result will be of the form:
 * 
 * if Alpha != 0 
 * then 
 *     if Alpha > 0
 *         V op1 Term/Alpha
 *     else
 *         V <op1 flipped> Term/Alpha 
 * else 
 *     0 op1 Term
 * 
 * </pre>
 * 
 * @author oreilly
 */
@Beta
public class IsolateUtil {	
	public static Expression isolate(Expression inequality, Expression linearVariableToIsolate) {
		if (inequality.getFunctor() == null || inequality.numberOfArguments() != 2) {
			throw new IllegalArgumentException("Not an inequality: "+inequality);
		}
		
		Expression leftExpression  = inequality.get(0);
		Expression operator        = inequality.getFunctor();
		Expression rightExpression = inequality.get(1);
		
		Expression result = isolate(leftExpression, operator, rightExpression, linearVariableToIsolate);
		
		return result;
	}
	
	public static Expression isolate(Expression leftExpression, Expression operator, Expression rightExpression, Expression linearVariableToIsolate) {
		Polynomial leftPolynomial  = DefaultPolynomial.make(leftExpression);
		Polynomial rightPolynomial = DefaultPolynomial.make(rightExpression);
		
		Expression result = isolate(leftPolynomial, operator, rightPolynomial, linearVariableToIsolate);
		
		return result;
	}

	public static Expression isolate(Polynomial leftPolynomial, Expression operator, Polynomial rightPolynomial, Expression linearVariableToIsolate) {		
		assertSupportedOperator(operator);
		
		Pair<List<Monomial>, List<Monomial>> splitIntoSimilarAndDissimilarTerms = splitIntoSimilarAndDissimilarTerms(leftPolynomial, linearVariableToIsolate);
		List<Monomial> leftSimilarTerms     = splitIntoSimilarAndDissimilarTerms.first;
		List<Monomial> leftDissimilarTerms  = splitIntoSimilarAndDissimilarTerms.second;  
		splitIntoSimilarAndDissimilarTerms  = splitIntoSimilarAndDissimilarTerms(rightPolynomial, linearVariableToIsolate);
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
		Polynomial rightDissimilarPolynomial = DefaultPolynomial.make(Plus.make(new ArrayList<Expression>(rightDissimilarTerms)));

		Monomial isolated = null;		
		// Compute Alpha
		List<Expression> alphaTerms = new ArrayList<>();
		for (Monomial similarTerm : leftSimilarPolynomial.getOrderedSummands()) {
			List<Expression> dissimilarFactors = new ArrayList<>();
			dissimilarFactors.add(Expressions.makeSymbol(similarTerm.getNumericConstantFactor()));
			for (Expression factor : similarTerm.getOrderedNonNumericFactors()) {
				if (factor.equals(linearVariableToIsolate)) {
					if (isolated == null) {
						isolated = DefaultMonomial.make(Exponentiation.make(factor, Expressions.makeSymbol(similarTerm.getPowerOfFactor(factor))));
					}
					if (!Rational.ONE.equals(similarTerm.getPowerOfFactor(factor))) {
						throw new IllegalArgumentException("Only linear (i.e. x^1) can be isolated by this API but found : "+factor);
					}
				}
				else {
					dissimilarFactors.add(Exponentiation.make(factor, Expressions.makeSymbol(similarTerm.getPowerOfFactor(factor))));
				}
			}
			alphaTerms.add(DefaultMonomial.make(Times.make(dissimilarFactors)));
		}
		Polynomial alpha = DefaultPolynomial.make(Plus.make(alphaTerms), rightDissimilarPolynomial.getVariables());
					
		Expression rightDissimilarTerm;
		if (alpha.equals(Expressions.ZERO)) {
			isolated            = DefaultMonomial.ZERO;
			rightDissimilarTerm = rightDissimilarPolynomial;
		}
		else {
			Pair<Polynomial, Polynomial> quotientAndRemainder = rightDissimilarPolynomial.divide(alpha);
			if (quotientAndRemainder.second.equals(Expressions.ZERO)) {
				rightDissimilarTerm = quotientAndRemainder.first;
			}
			else {
				rightDissimilarTerm = Division.simplify(Division.make(rightDissimilarPolynomial, alpha));
			}
		}
		
		Expression result;
		// If alpha is numeric we can divide (or know it was 0 - handled accordingly already).
		if (Expressions.isNumber(alpha)) {
			result =  Expressions.apply(flipIfRequired(operator, alpha), isolated, rightDissimilarTerm);
		}
		else {
			Expression thenBranch;
			if (isEquality(operator)) {
				thenBranch = Expressions.apply(operator, isolated, rightDissimilarTerm);
			}
			else {
				thenBranch = IfThenElse.make(Expressions.apply(FunctorConstants.GREATER_THAN, alpha, Expressions.ZERO), 
								Expressions.apply(operator, isolated, rightDissimilarTerm), 
								Expressions.apply(flipInequalityOperator(operator), isolated, rightDissimilarTerm));
			}
			result = IfThenElse.make(Expressions.apply(FunctorConstants.DISEQUALITY, alpha, Expressions.ZERO), 
					thenBranch, 
					Expressions.apply(FunctorConstants.EQUALITY, Expressions.ZERO, rightDissimilarPolynomial));
		}
		
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
	
	public static Expression flipIfRequired(Expression operator, Polynomial alpha) {
		// Determine if the operator needs to be flipped
		Expression result = operator;
		if (!isEquality(operator)) {
			Rational alphaCoefficientProduct = Rational.ONE;
			for (Monomial alphaTerm : alpha.getOrderedSummands()) {
				alphaCoefficientProduct = alphaCoefficientProduct.multiply(alphaTerm.getNumericConstantFactor());
			}
			if (alphaCoefficientProduct.isNegative()) {
				result = flipInequalityOperator(operator);
			}				
		}
		return result;
	}
	
	public static boolean isEquality(Expression operator) {
		boolean result = operator.equals(FunctorConstants.EQUALITY) || operator.equals(FunctorConstants.DISEQUALITY);
		return result;
	}
	
	public static Expression flipInequalityOperator(Expression operator) {
		Expression result;
		
		if (operator.equals(FunctorConstants.LESS_THAN)) {
			result = Expressions.makeSymbol(FunctorConstants.GREATER_THAN);
		}
		else if (operator.equals(FunctorConstants.LESS_THAN_OR_EQUAL_TO)) {
			result = Expressions.makeSymbol(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
		}
		else if (operator.equals(FunctorConstants.GREATER_THAN)) {
			result = Expressions.makeSymbol(FunctorConstants.LESS_THAN);
		}
		else if (operator.equals(FunctorConstants.GREATER_THAN_OR_EQUAL_TO)) {
			result = Expressions.makeSymbol(FunctorConstants.LESS_THAN_OR_EQUAL_TO);
		}
		else {
			throw new IllegalArgumentException("operator "+operator+" is not an inequality");
		}

		return result;
	}
}