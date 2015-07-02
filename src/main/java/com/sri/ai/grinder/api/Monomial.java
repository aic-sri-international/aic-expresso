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
package com.sri.ai.grinder.api;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * A monomial is a product of a number and powers of variables, where the number
 * constant (if any) is the first argument and the variables are in alphabetical
 * order.<br>
 * 
 * Examples: <br>
 * 
 * <pre>
 * <code>
 * 3*x^2
 * y
 * y^2
 * 10
 * 4*x^3*y^2
 * 150*x^2
 * </code>
 * </pre>
 * 
 * <b>NOTE:</b> To make the library as general as possible, any expression
 * that is not a numeric constant or an application of +, -, *, / or ^ with
 * constant integer exponent, is to be considered a "generalized variable".
 * Therefore:
 * 
 * <pre>
 * <code>
 * vars(10 * |Dogs|^2 * |People|^3 * f(y) * x^2) = { |Dogs|, |People|, f(y), x }
 * </code>
 * </pre>
 * 
 * @author oreilly
 *
 */
@Beta
public interface Monomial extends FunctionApplication {
	/**
	 * 
	 * @return the numerical constant (i.e. coefficient) of the monomial.
	 */
	Rational getCoefficient();
	
	/**
	 * Convenience routine for getting the variables of the monomial in Set form.
	 * 
	 * @return a Set representation of the variables contained within the Monomial.
	 */
	Set<Expression> getVariables();

	/**
	 * Get a unique list, lexicographically ordered, of the variables in the monomial.
	 * 
	 * @return a lexicographically ordered list of the variables contained in
	 *         the monomial.
	 */
	List<Expression> getVariablesLexicographicallyOrdered();

	/**
	 * 
	 * @return the powers of the variables in the monomial, which map to the
	 *         lexicographical order of the variables.
	 */
	List<Rational> getPowersOfLexicographicallyOrderedVariables();
	
	/**
	 * Get the power of the given variable if it is a variable of the monomial
	 * otherwise return 0.
	 * 
	 * @param variable
	 *            the variable whose power is to be retrieved.
	 * @return the power of the given variable if the variable is contained in
	 *         the monomial. If not contained in the monomial will return 0.
	 */
	Rational getPowerOfVariable(Expression variable);

	/**
	 * The <b>signature</b> of a monomial <em>M</em> wrt a tuple of variables
	 * <em>V</em> is the tuple of the powers of variables in <em>V</em> in
	 * <em>M</em> (the power is 0 if the variable is not present in <em>M</em>).<br>
	 * Examples:<br>
	 * 
	 * <pre>
	 * <code>
	 * signature(3 * y * x^2,  (x, y, z))  =  (2, 1, 0)
	 * signature(3,            (x, y, z))  =  (0, 0, 0)
	 * </code>
	 * </pre>
	 * 
	 * @param variables
	 *            a list of variables.
	 * @return the tuple of the powers of the given variables that are in this
	 *         Monomial (0 is returned for the power of variables not present in
	 *         this monomial).
	 */
	default List<Rational> getSignature(List<Expression> variables) {
		List<Rational> result = new ArrayList<>(variables.size());
		variables.forEach(variable -> result.add(getPowerOfVariable(variable)));
		return result;
	}

	/**
	 * Two monomials <em>M1</em> and <em>M2</em> have <b>like terms</b> if they
	 * contain the same variables raised to the same powers, i.e. if vars(M1) =
	 * vars(M2) and signature(M1, vars(M1)) = signature(M2, vars(M2)).<br>
	 * Have Like Terms Examples:<br>
	 * 
	 * <pre>
	 * <code>
	 *  x^2 * y and 3 * y * x^2
	 *  x and 3 * x
	 *  10 and 20
	 * </code>
	 * </pre>
	 * 
	 * @param monomial
	 *            the monomial to check if have like terms with.
	 * @return true if this monomial has like terms with the given monomial,
	 *         false otherwise.
	 */
	default boolean haveLikeTerms(Monomial other) {
		boolean result = getVariablesLexicographicallyOrdered().equals(other.getVariablesLexicographicallyOrdered())
				&& getPowersOfLexicographicallyOrderedVariables().equals(other.getPowersOfLexicographicallyOrderedVariables());
		return result;
	}

	/**
	 * The degree of a monomial is the sum of the exponents of all its variables.
	 * 
	 * @return the degree of the monomial.
	 */
	default Rational degree() {
		Rational result = getPowersOfLexicographicallyOrderedVariables().stream().reduce(Rational.ZERO,
				(r1, r2) -> r1.add(r2));
		return result;
	}

	/**
	 * Multiply this Monomial by another.
	 * 
	 * @param multiplier
	 *            the multiplier.
	 * @return a new Monomial representing the result of the multiplication.
	 */
	Monomial times(Monomial multiplier);

	/**
	 * Divide this monomial with another.
	 * 
	 * @param divisor
	 *            the divisor
	 * @return a pair consisting of the quotient and remainder of the division.
	 */
	Pair<Monomial, Monomial> divide(Monomial divisor);
	
	/**
	 * Raise this monomial to a given power.
	 * 
	 * @param exponent
	 *            a non negative integer exponent to raise the monomial to.
	 * @return a new monomial which is the result of raising this monomial to
	 *         the given exponent.
	 * @throws IllegalArgumentException
	 *         if the exponent is negative.
	 */
	Monomial exponentiate(int exponent);
	
	/**
	 * Create a lexicographically ordered union (no duplicates) of the variables
	 * contained in two Monomials.
	 * 
	 * @param m1
	 *            the first monomial.
	 * @param m2
	 *            the second monomial.
	 * @return the lexicographically ordered union (no duplicates) of the
	 *         variables contained in two Monomials.
	 */
	public static List<Expression> unionVariablesLexicographically(Monomial m1, Monomial m2) {
		List<Expression> m1Variables = m1.getVariablesLexicographicallyOrdered();
		List<Expression> m2Variables = m2.getVariablesLexicographicallyOrdered();
		// For efficiency ensure we have enough capacity in the union up front.
		List<Expression> result = new ArrayList<>(m1Variables.size()
				+ m2Variables.size());

		// NOTE: we know m1 and m2's variables are lexicographically ordered,
		// which we can take advantage of to perform the lexicraphically ordered
		// union more efficiently
		int m1VariablesSize = m1Variables.size();
		int m2VariablesSize = m2Variables.size();
		Expression m1Var, m2Var;
		for (int i1 = 0, i2 = 0; i1 < m1VariablesSize || i2 < m2VariablesSize;) {
			if (i1 < m1VariablesSize && i2 < m2VariablesSize) {
				m1Var = m1Variables.get(i1);
				m2Var = m2Variables.get(i2);
				int m1VarComparisonToM2Var = m1Var.compareTo(m2Var);

				if (m1VarComparisonToM2Var == 0) { // are considered equal
					// just add one of them (as considered same) as do not want
					// duplicates in the returned union.
					result.add(m1Var);
					i1++;
					i2++;
				} 
				else if (m1VarComparisonToM2Var < 0) { // i.e. m1Var is before m2Var
					result.add(m1Var);
					i1++;
				} 
				else { // i.e. m2Var is before m1Var
					result.add(m2Var);
					i2++;
				}
			} 
			else if (i1 < m1VariablesSize) { // implies i2 is exhausted
				result.add(m1Variables.get(i1));
				i1++;
			} 
			else { // implies i2 < m2VariableSize and i1 is exhausted
				result.add(m2Variables.get(i2));
				i2++;
			}
		}

		return result;
	}
}