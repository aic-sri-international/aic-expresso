/*
 * Copyright (c) 2015, SRI International
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
package com.sri.ai.grinder.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.api.Polynomial;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.math.BernoulliNumber;
import com.sri.ai.util.math.Rational;

/**
 * Utility class for computing the summation of a given polynomial. 
 * Logic based on <a href="https://en.wikipedia.org/wiki/Faulhaber%27s_formula">Faulhaber's Formula</a>.
 * 
 * @author oreilly
 *
 */
@Beta
public class PolynomialSummation {
	private static final Expression PLUS_FUNCTOR     = Expressions.makeSymbol(FunctorConstants.PLUS);
	private static final Expression MINUS_FUNCTOR    = Expressions.makeSymbol(FunctorConstants.MINUS);
	private static final Expression TIMES_FUNCTOR    = Expressions.makeSymbol(FunctorConstants.TIMES);
	private static final Expression DIVISION_FUNCTOR = Expressions.makeSymbol(FunctorConstants.DIVISION);
	
	/**
	 * Compute the sum for the summation of a polynomial.
	 * 
	 * @param indexOfSummation
	 *        the index variable of the summation.
	 * @param lowerBoundExclusive
	 *        the lower bound of the summation.
	 * @param upperBoundInclusive
	 *        the upper bound of the summation.
	 * @param summand
	 *        the polynomial to be summed.
	 * @return the sum of the given summation.
	 */
	public static Polynomial sum(Expression indexOfSummation, Expression lowerBoundExclusive, Expression upperBoundInclusive, Polynomial summand) {
		Polynomial result;
		
		List<Expression> factors                 = Arrays.asList(indexOfSummation);
		Set<Expression> subsetOfSignatureFactors = new HashSet<Expression>();
		subsetOfSignatureFactors.addAll(factors);
		
		Polynomial projectedSummand = summand.project(subsetOfSignatureFactors);
		int        n                = projectedSummand.degree();
		
		//
		// collect the t coefficients
		List<Expression> tCoefficients = new ArrayList<>(n);
		for (int i = 0; i <= n; i++) {
			tCoefficients.add(Expressions.ZERO);
		}
		for (int i = 0; i < projectedSummand.numberOfTerms(); i++) {
			Monomial term = projectedSummand.getOrderedSummands().get(i);
			tCoefficients.set(term.getPowerOfFactor(indexOfSummation).intValue(), term.getCoefficient(factors));
		}
		
		//
        // compute polynomials R_i(x) = (x + l)^i for each i
		Expression indexOfSummationPlusLowerBound           = new DefaultFunctionApplication(PLUS_FUNCTOR, Arrays.asList(indexOfSummation, lowerBoundExclusive));
		Polynomial indexOfSummationPlusLowerBoundPolynomial = DefaultPolynomial.make(indexOfSummationPlusLowerBound, factors);

		List<Polynomial> rPolynomials = new ArrayList<>(n);		
		rPolynomials.add(DefaultPolynomial.make(Expressions.ONE, factors));
		rPolynomials.add(indexOfSummationPlusLowerBoundPolynomial);		
		for (int i = 2; i <= n; i++) {
			rPolynomials.add(rPolynomials.get(i-1).times(indexOfSummationPlusLowerBoundPolynomial));
		}	
		Map<Pair<Integer, Integer>, Expression> indexedRCoefficient = new LinkedHashMap<>();
		for (int i = 0; i <= n; i++) {
			Polynomial rPolynomial = rPolynomials.get(i);
			for (int q = 0; q <= i; q++) {
				Pair<Integer, Integer> indexKey = new Pair<>(i, q);
				Monomial rqxq = rPolynomial.getSignatureTermMap().get(Arrays.asList(new Rational(q)));
				if (rqxq == null) {
					indexedRCoefficient.put(indexKey, Expressions.ZERO);
				}
				else {
					indexedRCoefficient.put(indexKey, rqxq.getCoefficient(factors));
				}
			}
		}
		
	
		//
		// compute "constants" (may contain variables other than x)  
		// s_i,q,j =   t_i*R_{i,q}/(q+1) (-1)^j choose(q+1,j) B_j
        // where R_{i,q}(x) is the coefficient in R_i(x) multiplying x^q.
		Map<Triple<Integer, Integer, Integer>, Polynomial> sConstants = new LinkedHashMap<>();
		for (int i = 0; i <= n; i++) {
			Expression ti = tCoefficients.get(i);
			for (int q = 0; q <= i; q++) {
				Expression riq     = indexedRCoefficient.get(new Pair<>(i, q));
				Expression tiByriq = new DefaultFunctionApplication(TIMES_FUNCTOR, Arrays.asList(ti, riq));
				for (int j = 0; j <= q; j++) {
					Triple<Integer, Integer, Integer> indexKey = new Triple<>(i, q, j);	
					Expression qPlus1        = Expressions.makeSymbol(q+1);
					Expression minus1PowerJ  = Expressions.makeSymbol(j % 2 == 0 ? 1 : -1);
					Expression chooseQplus1J = Expressions.makeSymbol(Util.binomialCoefficient(q+1, j));
					Expression bernoulliJ    = Expressions.makeSymbol(BernoulliNumber.computeFirst(j));					
					Expression sConstant = new DefaultFunctionApplication(TIMES_FUNCTOR, Arrays.asList(
								new DefaultFunctionApplication(DIVISION_FUNCTOR, Arrays.asList(tiByriq, qPlus1)),
								minus1PowerJ,
								chooseQplus1J,
								bernoulliJ
							));
					
					sConstants.put(indexKey, DefaultPolynomial.make(sConstant, factors));
				}
			}
		}

		//
		// compute polynomials, for each q, j,   V_{q + 1 -j}  = (u - l)^{q + 1 - j}
		Expression upperBoundMinusLowerBound            = new DefaultFunctionApplication(MINUS_FUNCTOR, Arrays.asList(upperBoundInclusive, lowerBoundExclusive));
		Polynomial upperBoundMinusLowerBoundPolynomial  = DefaultPolynomial.make(upperBoundMinusLowerBound, factors);
		Map<Integer, Polynomial> vValues                = new LinkedHashMap<>();
		for (int q = 0; q <= n; q++) {
			for (int j = 0; j <= q; j++) {
				Integer exponent = q + 1 - j;
				if (!vValues.containsKey(exponent)) {
					vValues.put(exponent, upperBoundMinusLowerBoundPolynomial.exponentiate(exponent));
				}
			}
		}
	
		//
		// Compute the w values and construct the final result.
		Polynomial ws = DefaultPolynomial.make(Expressions.ZERO, factors);
		for (int i = 0; i <= n; i++) {
			for (int q = 0; q <= i; q++) {
				for (int j = 0; j <= q; j++) {
					Triple<Integer, Integer, Integer> sConstantKey = new Triple<>(i, q, j);
					Integer                           valueKey     = q + 1 - j;
					Polynomial sConstant = sConstants.get(sConstantKey);
					Polynomial vValue    = vValues.get(valueKey);
					Polynomial w         = sConstant.times(vValue);
					ws = ws.add(w);
				}
			}
		}	
		
		List<Expression> generalizedVariables = DefaultPolynomial.extractGeneralizedVariables(ws);
		if (generalizedVariables.size() > 0) {
			// Simplify in the context of the contained generalized variables 
			// and then return as a single constant factor (i.e. the index variable should not be present).
			ws = DefaultPolynomial.make(ws, generalizedVariables);
		}
		
		result = DefaultPolynomial.make(ws, factors);
		
		return result;
	}
}
