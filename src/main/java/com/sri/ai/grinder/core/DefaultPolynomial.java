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

import static com.sri.ai.grinder.core.DefaultMonomial.isLegalExponent;
import static com.sri.ai.grinder.core.DefaultMonomial.simplifyExponentIfPossible;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.api.Polynomial;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.base.Pair;

/**
 * Default implementation of the Polynomial interface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultPolynomial extends DefaultFunctionApplication implements
		Polynomial {
	//
	public static final Expression POLYNOMIAL_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.PLUS);
	//
	private static final long serialVersionUID = 1L;
	//
	private static final Expression PLUS_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.PLUS);
	private static final Expression MINUS_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.MINUS);
	private static final Expression TIMES_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.TIMES);
	private static final Expression DIVISION_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.DIVISION);
	private static final Expression EXPONENTIATION_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.EXPONENTIATION);
	//
	private List<Expression> signatureFactors = null;
	private List<Monomial> orderedSummands = null;
	private Set<Expression> nonNumericConstantFactors = null;

	public static Polynomial make(Expression expression,
			List<Expression> signatureFactors) {
		Polynomial result = null;
		if (expression.hasFunctor(PLUS_FUNCTOR)) {
			// E1 + E2 --> make(E1, F).add(make(E2, F))
			// Note: as + can have 0 or more arguments we start with 0 (i.e. the
			// neutral element).
			result = makeFromMonomial(Expressions.ZERO, signatureFactors);
			for (Expression summandExpression : expression.getArguments()) {
				Polynomial summand = make(summandExpression, signatureFactors);
				result = result.add(summand);
			}
		} else if (expression.hasFunctor(MINUS_FUNCTOR)) {
			// E1 - E2 --> make(E1, F).minus(make(E2, F))
			if (expression.numberOfArguments() == 1) {
				// is '-(something)' which is equivalent to
				// -1 * (something)
				Polynomial p1 = makeFromMonomial(Expressions.MINUS_ONE,
						signatureFactors);
				Polynomial p2 = make(expression.get(0), signatureFactors);
				result = p1.times(p2);
			} else if (expression.numberOfArguments() == 2) {
				Polynomial p1 = make(expression.get(0), signatureFactors);
				Polynomial p2 = make(expression.get(1), signatureFactors);
				result = p1.minus(p2);
			} else {
				throw new IllegalArgumentException(
						"Not a legal minus expression: " + expression);
			}
		} else if (expression.hasFunctor(TIMES_FUNCTOR)) {
			// E1 * E2 --> make(E1, F).times(make(E2, F))
			// Note: as * can have 0 or more arguments we start with 1 (i.e. the
			// neutral element).
			result = makeFromMonomial(Expressions.ONE, signatureFactors);
			// Note: handle case when one of the multipliers is 0 (i.e. an
			// absorbing element).
			Polynomial absorbingElement = makeFromMonomial(Expressions.ZERO,
					signatureFactors);
			for (Expression multiplierExpression : expression.getArguments()) {
				Polynomial multiplier = make(multiplierExpression,
						signatureFactors);
				if (multiplier.equals(absorbingElement)) {
					// * by 0 so answer is 0, no need to worry about any other
					// multipliers.
					result = absorbingElement;
					break;
				} else {
					result = result.times(multiplier);
				}
			}
		} else if (expression.hasFunctor(DIVISION_FUNCTOR)) {
			// E1 / E2 --> make(E1, F).divide(make(E2, F))
			Polynomial p1 = make(expression.get(0), signatureFactors);
			Polynomial p2 = make(expression.get(1), signatureFactors);

			Pair<Polynomial, Polynomial> quotientAndRemainder = p1.divide(p2);
			Polynomial quotient = quotientAndRemainder.first;
			Polynomial remainder = quotientAndRemainder.second;
			Polynomial zero = makeFromMonomial(Expressions.ZERO,
					signatureFactors);
			if (remainder.equals(zero)) {
				throw new UnsupportedOperationException(
						"Constructing a polynomial form a division operation that results in a remainder is not supported: "
								+ expression
								+ "\nquotient ="
								+ quotient
								+ "\nremainder=" + remainder);
			}
		} else if (expression.hasFunctor(EXPONENTIATION_FUNCTOR)) {
			// E1 ^ m with m an integer constant --> make(E1).exponentiate(m)
			Expression base  = expression.get(0);
			Expression power = simplifyExponentIfPossible(expression.get(1));
			if (isLegalExponent(power)) {
				Polynomial p1 = make(base, signatureFactors);
				result = p1.exponentiate(power.intValue());
			}
		}

		// E is a single-factor Monomial.
		if (result == null) {
			result = makeFromMonomial(expression, signatureFactors);
		}

		return result;
	}

	//
	// START-Polynomial
	@Override
	public List<Expression> getSignatureFactors() {
		return signatureFactors;
	}

	@Override
	public Monomial asMonomial() throws IllegalStateException {
		if (!isMonomial()) {
			throw new IllegalStateException("This polynomial, " + this
					+ ", is not a monomial");
		}

		Monomial result = orderedSummands.get(0);

		return result;
	}

	@Override
	public List<Monomial> getOrderedSummands() {
		return orderedSummands;
	}

	@Override
	public Set<Expression> getNonNumericConstantFactors() {
		return nonNumericConstantFactors;
	}

	@Override
	public Polynomial add(Polynomial summand) throws IllegalArgumentException {
		return null; // TODO - implement
	}

	@Override
	public Polynomial minus(Polynomial subtrahend)
			throws IllegalArgumentException {
		return null; // TODO - implement
	}

	@Override
	public Polynomial times(Polynomial multiplier)
			throws IllegalArgumentException {
		return null; // TODO - implement
	}

	@Override
	public Pair<Polynomial, Polynomial> divide(Polynomial divisor)
			throws IllegalArgumentException {
		return null; // TODO - implement
	}

	@Override
	public Polynomial exponentiate(int exponent)
			throws IllegalArgumentException {
		return null; // TODO - implement
	}

	@Override
	public Polynomial project(Set<Expression> subsetOfSignatureFactors)
			throws IllegalArgumentException {
		return null; // TODO - implement
	}

	// END-Polynomial
	//

	//
	// START-FunctionApplication
	@Override
	public Expression set(int i, Expression newIthArgument) {
		List<Expression> signatureFactors = getSignatureFactors();
		Expression result = super.set(i, newIthArgument);
		// Ensure we make a Polynomial out ot the modified expression
		result = make(result, signatureFactors);
		return result;
	}

	// END-FunctionApplication
	//

	//
	// PRIVATE
	//
	private DefaultPolynomial(List<Monomial> orderedSummands,
			List<Expression> signatureFactors) {
		super(POLYNOMIAL_FUNCTOR, new ArrayList<>(orderedSummands));
		// NOTE: we use Collections.unmodifiable<...> to ensure Polynomials are
		// immutable.
		this.signatureFactors = Collections.unmodifiableList(signatureFactors);
		this.orderedSummands = Collections.unmodifiableList(orderedSummands);

		this.nonNumericConstantFactors = new LinkedHashSet<>();
		for (Monomial monomial : orderedSummands) {
			this.nonNumericConstantFactors.addAll(monomial
					.getOrderedNonNumericConstantFactors());
		}
		this.nonNumericConstantFactors = Collections
				.unmodifiableSet(this.nonNumericConstantFactors);
	}

	private static Polynomial makeFromMonomial(Expression monomialExpression,
			List<Expression> signatureFactors) {
		Monomial monomial = DefaultMonomial.make(monomialExpression);
//		Monomial coefficient = monomial.getCoefficient(new HashSet<>(signatureFactors));
//		if (!coefficient.equals(monomial)) {
//// TODO - need to apply signature to get correct set of constant factors.			
//			throw new UnsupportedOperationException("TODO - need to apply polynomials signature to get correct set of constant factors");
//		}
		
		Polynomial result = new DefaultPolynomial(
				Collections.singletonList(monomial), signatureFactors);

		return result;
	}
}
