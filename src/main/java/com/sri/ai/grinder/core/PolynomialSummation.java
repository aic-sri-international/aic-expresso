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
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.api.Polynomial;
import com.sri.ai.grinder.library.FunctorConstants;

@Beta
public class PolynomialSummation {
	public static void main(String[] args) {
		sum(Expressions.parse("x"), Expressions.parse("y + 1"), Expressions.parse("z + 2"), DefaultPolynomial.make(Expressions.parse("2 + 3*x^2"), Arrays.asList(Expressions.parse("x"))));
	}
	
	public static Polynomial sum(Expression indexOfSummation, Expression lowerBound, Expression upperBound, Polynomial summand) {
		Polynomial result;
		
		List<Expression> factors                 = Arrays.asList(indexOfSummation);
		Set<Expression> subsetOfSignatureFactors = new HashSet<Expression>();
		subsetOfSignatureFactors.addAll(factors);
		
		Polynomial projectedSummand = summand.project(subsetOfSignatureFactors);
		int        n                = projectedSummand.degree();
				
		List<Expression> tCoefficients = new ArrayList<>(n);
		for (int i = 0; i <= n; i++) {
			tCoefficients.add(Expressions.ZERO);
		}
		for (int i = 0; i < projectedSummand.numberOfTerms(); i++) {
			Monomial term = projectedSummand.getOrderedSummands().get(i);
			tCoefficients.set(term.degree(), term.getCoefficient(factors));
		}
		
System.out.println("tCoefficients="+tCoefficients);
		
        // compute polynomials R_i(x) = (x + l)^i for each i
		Expression indexOfSummationPlusLowerBound          = new DefaultFunctionApplication(Expressions.makeSymbol(FunctorConstants.PLUS), Arrays.asList(indexOfSummation, lowerBound));
		Polynomial indexOfSummationPlusLowerBoundPolynomial = DefaultPolynomial.make(indexOfSummationPlusLowerBound, factors);

		List<Polynomial> rPolynomials = new ArrayList<>(n);		
		rPolynomials.add(DefaultPolynomial.make(Expressions.ONE, factors));
		rPolynomials.add(indexOfSummationPlusLowerBoundPolynomial);
		for (int i = 2; i <= n; i++) {
			rPolynomials.add(rPolynomials.get(i-1).times(indexOfSummationPlusLowerBoundPolynomial));
		}
System.out.println("rPolynomials="+rPolynomials);
		
		result = null; // TODO - compute correctly
		return result;
	}
}
