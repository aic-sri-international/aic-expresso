package com.sri.ai.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.library.number.Plus;
import com.sri.ai.grinder.sgdpllt.library.number.UnaryMinus;
import com.sri.ai.util.collect.NestedIterator;

/** 
 * A specialized representation of a difference arithmetic literal side
 * that provides the positive, negative and constant terms in it.
 * @author braz
 *
 */
public class DifferenceArithmeticLiteralSide {
	private Set<Expression> positives;
	private Set<Expression> negatives;
	private int constant;
	
	public DifferenceArithmeticLiteralSide() {
		this(new LinkedHashSet<>(), new LinkedHashSet<>(), 0);
	}
	
	public DifferenceArithmeticLiteralSide(Set<Expression> positiveTerms, Set<Expression> negativeTerms, int constant) {
		this.positives = positiveTerms;
		this.negatives = negativeTerms;
		this.constant  = constant;
	}
	
	/**
	 * Given a sum, or an expression to be interpreted as a single-term sum,
	 * returns a triple containing a multiset of positive terms, a multiset of negative terms,
	 * and the sum of all numerical constants in it.
	 * If <code>makeDuplicateError</code> is non-null and a duplicate term is found,
	 * the duplicate is passed to it as a parameter and the resulting Error is thrown.
	 * @param expression
	 * @return
	 */
	public DifferenceArithmeticLiteralSide(Expression expression) {

		positives = new LinkedHashSet<>();
		negatives = new LinkedHashSet<>();
		constant = 0;

		List<Expression> arguments = Plus.getSummands(expression);
		for (Expression argument : arguments) {
			if (argument.hasFunctor(MINUS) && argument.numberOfArguments() == 1) {
				argument = UnaryMinus.simplify(argument); // removes double minuses
			}
			if (argument.hasFunctor(MINUS)) {
				Expression negationArgument = argument.get(0);
				if (negationArgument.getValue() instanceof Number) {
					constant = constant - ((Number) negationArgument.getValue()).intValue(); // note the -  !
				}
				else {
					if (negatives.contains(negationArgument)) {
						throw new DuplicateError(expression, negationArgument);
					}
					else if (positives.contains(negationArgument)) {
						positives.remove(negationArgument); // cancel out with the positive one, and don't add it to negatives
					}
					else {
						negatives.add(negationArgument);
					}
				}
			}
			else {
				if (argument.getValue() instanceof Number) {
					constant = constant + ((Number) argument.getValue()).intValue(); // note the +  !
				}
				else {
					if (positives.contains(argument)) {
						throw new DuplicateError(expression, argument);
					}
					else if (negatives.contains(argument)) {
						negatives.remove(argument); // cancel out with the negative one, and don't add it to positives
					}
					else {
						positives.add(argument);
					}
				}
			}
		}
	}
	
	public Set<Expression> getPositives() {
		return positives;
	}

	public Set<Expression> getNegatives() {
		return negatives;
	}

	public int getConstant() {
		return constant;
	}

	/**
	 * Given two difference arithmetic tuples, each containing positive and negative terms and a numeric constant in a summation,
	 * returns another tuple of the same form representing their subtraction,
	 * or throws an Error if any of the terms appears with the same final sign multiple times
	 * (which would require representing a multiple of it), such as in ({X}, {}, 1) - ({}, {X}, 2)
	 * which would result in 2*X - 1.
	 * @param numericalComparison TODO
	 * @param positiveAndNegativeTermsAndConstant1
	 * @param positiveAndNegativeTermsAndConstant2
	 * @return
	 * @throws Error
	 */
	static DifferenceArithmeticLiteralSide subtractDifferenceArithmeticLiteralSides(
			Expression numericalComparison, 
			DifferenceArithmeticLiteralSide positiveAndNegativeTermsAndConstant1, 
			DifferenceArithmeticLiteralSide positiveAndNegativeTermsAndConstant2) {

		// cancel out terms that are positive in both first and second sides (they cancel because second parts is being subtracted):
		Iterator<Expression> positive1Iterator = positiveAndNegativeTermsAndConstant1.positives.iterator();
		while (positive1Iterator.hasNext()) {
			Expression positive1 = positive1Iterator.next();
			if (positiveAndNegativeTermsAndConstant2.positives.contains(positive1)) {
				positive1Iterator.remove();
				positiveAndNegativeTermsAndConstant2.positives.remove(positive1);
			}
		}
		
		// cancel out terms that are negative in both first and second parts (they cancel because second parts is being subtracted):
		Iterator<Expression> negative1Iterator = positiveAndNegativeTermsAndConstant1.negatives.iterator();
		while (negative1Iterator.hasNext()) {
			Expression negative1 = negative1Iterator.next();
			if (positiveAndNegativeTermsAndConstant2.negatives.contains(negative1)) {
				negative1Iterator.remove();
				positiveAndNegativeTermsAndConstant2.negatives.remove(negative1);
			}
		}
		
		Set<Expression> unionOfPositiveTerms = new LinkedHashSet<>();
		Iterable<Expression> positiveTerms = in(new NestedIterator<Expression>(
				positiveAndNegativeTermsAndConstant1.positives,
				positiveAndNegativeTermsAndConstant2.negatives)); // negative terms in second tuple are actually positive since it is being subtracted
		
		for (Expression positive : positiveTerms) {
			boolean noDuplicate = unionOfPositiveTerms.add(positive);
			boolean duplicate = ! noDuplicate;
			if (duplicate) {
				throw new DuplicateError(numericalComparison, positive);
			}
			else if (unionOfPositiveTerms.size() == 2) {
				throw new Error(numericalComparison + " is not a difference arithmetic atom because it contains more than one positive term when moved to the left-hand side: " + join(unionOfPositiveTerms));
			}
		}
		
		Set<Expression> unionOfNegativeTerms = new LinkedHashSet<>();
		Iterable<Expression> negativeTerms = in(new NestedIterator<Expression>(
				positiveAndNegativeTermsAndConstant1.negatives,
				positiveAndNegativeTermsAndConstant2.positives)); // positive terms in second tuple are actually negative since it is being subtracted
		
		for (Expression negative : negativeTerms) {
			boolean noDuplicate = unionOfNegativeTerms.add(negative);
			boolean duplicate = ! noDuplicate;
			if (duplicate) {
				throw new DuplicateError(numericalComparison, negative);
			}
			else if (unionOfNegativeTerms.size() == 2) {
				throw new Error(numericalComparison + " is not a difference arithmetic atom because it contains more than one negative term when moved to the left-hand side: " + join(unionOfNegativeTerms));
			}
		}
		
		int constant = positiveAndNegativeTermsAndConstant1.constant - positiveAndNegativeTermsAndConstant2.constant;
	
		DifferenceArithmeticLiteralSide result = new DifferenceArithmeticLiteralSide(unionOfPositiveTerms, unionOfNegativeTerms, constant);
		return result;
	}
	
	/**
	 * An error during the construction of a {@link DifferenceArithmeticLiteralSide}
	 * indicating that a term is duplicate (with the same sign), thus
	 * making the given expression impossible to represent as a {@link DifferenceArithmeticLiteralSide}
	 * because the term needs to be multiplied by a constant.
	 * 
	 * @param numericalComparison
	 * @param duplicate
	 *
	 * @author braz
	 *
	 */
	public static class DuplicateError extends Error {
		private static final long serialVersionUID = 1L;

		public DuplicateError(Expression numericalComparison, Expression duplicate) {
			super(numericalComparison + " is not a difference arithmetic atom because " 
					+ duplicate + " sums with itself, but no multiples are allowed in difference arithmetic");
		}
	}
	
}