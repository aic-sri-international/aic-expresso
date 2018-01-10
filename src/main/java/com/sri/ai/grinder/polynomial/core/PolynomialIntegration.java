package com.sri.ai.grinder.polynomial.core;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.util.math.Rational;

/**
 * Utility class for performing 
 * <a href="http://hyperphysics.phy-astr.gsu.edu/hbase/intpol.html">indefinite and definite integration on polynomials</a>.
 * 
 * @author oreilly
 *
 */
@Beta
public class PolynomialIntegration {

	/**
	 * Takes a polynomial:<br>
	 * <pre>
	 * t_1 * variable ^ n + ... + t_n * variable + t_{n+1}
	 * </pre>
	 * and returns the polynomial equivalent to:<br>
	 * <pre>
	 * (t_1/(n + 1)) * variable ^ {n + 1} + ... + (t_n / 2) * variable^2 + t_{n+1}*variable
	 * </pre>
	 * 
	 * @param polynomial
	 *        the polynomial the indefinite integral is to be found for.
	 * @param variable
	 *        the variable integration is with respect to.
	 * @return the indefinite integral of the given polynomial.
	 */
	public static Polynomial indefiniteIntegral(Polynomial polynomial, Expression variable) {				
		List<Expression> variables = new ArrayList<>(polynomial.getVariables());
		if (!variables.contains(variable)) {
			variables.add(variable);
			// Ensure the variable we are integrating with respect to is 
			// recognized by the polynomial as being one (this is implied).
			try {
			polynomial = DefaultPolynomial.make(polynomial, variables);
			}
			catch(IllegalArgumentException exception) {
				throw new IllegalArgumentException("Integrating " + polynomial + " requires it to be an integral in the integral variable " + variable + " but it is not because " + exception);
			}
		}		
		
		// Get the integrals of its terms
		List<Expression> integralsOfTerms = new ArrayList<>();
		for (Monomial term : polynomial.getMonomials()) {
			// indefinite integral of the term is:
			// &#x222b; a*x^n dx = a*(x^(n+1)/(n+1) + C		
			// NOTE: we do not need to worry about the case where n = -1 (i.e. division by zero case)
			// as our support for Polynomials only allows for positive integer exponents.
			List<Expression> factorsOfIntegral = new ArrayList<>();
			boolean variableFactorAdded = false;
			for (Expression factor : term.getFactors()) {
				Rational powerOfFactor = term.getPowerOfFactor(factor);
				if (factor.equals(variable)) {					
					Expression nPlusOne = Expressions.makeSymbol(powerOfFactor.add(1));
					factorsOfIntegral.add(Division.make(Exponentiation.make(variable, nPlusOne), nPlusOne));
					variableFactorAdded = true;
				}
				else {
					factorsOfIntegral.add(Exponentiation.make(factor, powerOfFactor));
				}
			}
			// Handle the case where the variable is not in the term, i.e.:
			// &#x222b; a dx = a*x + C	
			if (!variableFactorAdded) {				
				factorsOfIntegral.add(variable);
			}
			
			integralsOfTerms.add(DefaultMonomial.make(Times.make(factorsOfIntegral)));
		}
		
		// The integral of any polynomial is the sum of the integrals of its terms.
		Polynomial result = DefaultPolynomial.make(Plus.make(integralsOfTerms), variables);
				
		return result;
	}
	
	/**
	 * This method will return the polynomial equivalent to:<br>
	 * <pre>
	 * Q.replace(variable, end) - Q.replace(variable, start)
	 * </pre>
	 * where 'Q = indefiniteIntegral(polynomial, variable)'
	 *  
	 * @param polynomial
	 *        the polynomial the definite integral is to be found for.
	 * @param variable
	 *        the variable integration is with respect to.
	 * @param start
	 *        the starting limit of the integral.
	 * @param end
	 *        the ending limit of the integral.
	 * @return the definite integral of the polynomial for the given limits.
	 */
	public static Polynomial definiteIntegral(Polynomial polynomial, Expression variable, Expression start, Expression end, Predicate<Expression> isVariable) {
		Polynomial q = indefiniteIntegral(polynomial, variable);
		
		Set<Expression> variableSet = new LinkedHashSet<>(q.getVariables()); // Note: will include variable due to calling indefiniteIntegral
		variableSet.addAll(Expressions.freeVariables(start, isVariable));
		variableSet.addAll(Expressions.freeVariables(end, isVariable));
//		if (!isNumber(start) && !isPositiveOrNegativeInfinity(start)) {
//			variableSet.add(start);
//		}
//		if (!isNumber(end) && !isPositiveOrNegativeInfinity(end)) {
//			variableSet.add(end);
//		}		
		List<Expression> variables = new ArrayList<>(variableSet);
		
		Polynomial minuendPolynomial    = replaceFactor(q, variable, end, variables);
		Polynomial subtrahendPolynomial = replaceFactor(q, variable, start, variables);
		
		Polynomial result = minuendPolynomial.minus(subtrahendPolynomial);
		
		return result;
	}
	
	private static Polynomial replaceFactor(Polynomial q, Expression variable, Expression value, List<Expression> variablesToIncludeInResult) {
		List<Expression> replacedTerms = replaceVariableByValueInTerms(q, variable, value);
		Polynomial result = DefaultPolynomial.make(Plus.make(replacedTerms), variablesToIncludeInResult);
		return result;
	}

	private static List<Expression> replaceVariableByValueInTerms(Polynomial q, Expression variable, Expression value) {
		List<Expression> replacedTerms = new ArrayList<>();
		for (Monomial term : q.getMonomials()) {
			List<Expression> replacedFactorsInTerm = replaceVariableByValueInTerm(term, variable, value);			
			replacedTerms.add(DefaultMonomial.make(Times.make(replacedFactorsInTerm)));
		}
		return replacedTerms;
	}

	private static List<Expression> replaceVariableByValueInTerm(Monomial term, Expression variable, Expression value) {
		List<Expression> replacedFactorsInTerm = new ArrayList<>();
		for (Expression factor : term.getFactors()) {
			replaceFactor(factor, variable, value, term, replacedFactorsInTerm);
		}
		return replacedFactorsInTerm;
	}

	private static void replaceFactor(Expression factor, Expression variable, Expression value, Monomial term, List<Expression> replacedFactorsInTerm) {
		Rational powerOfFactor = term.getPowerOfFactor(factor);
		if (factor.equals(variable)) {					
			replacedFactorsInTerm.add(Exponentiation.make(value, powerOfFactor));
		}
		else {
			replacedFactorsInTerm.add(Exponentiation.make(factor, powerOfFactor));
		}
	}
}
