package com.sri.ai.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.library.number.BinaryMinus;
import com.sri.ai.grinder.sgdpllt.library.number.Plus;
import com.sri.ai.grinder.sgdpllt.library.number.UnaryMinus;
import com.sri.ai.util.collect.NestedIterator;

/** 
 * A specialized {@link Expression} representation of a difference arithmetic literal side
 * (valid of the other side is zero)
 * that provides the positive, negative and constant terms in it.
 * @author braz
 *
 */
public class DifferenceArithmeticLiteralSide extends AbstractExpressionWrapper {
	
	private static final long serialVersionUID = 1L;

	private Set<Expression> positives;
	private Set<Expression> negatives;
	private int constant;
	
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
	 * @param comparisonSide
	 * @return
	 */
	public DifferenceArithmeticLiteralSide(Expression comparisonSide) throws DuplicateTermException {

		positives = new LinkedHashSet<>();
		negatives = new LinkedHashSet<>();
		constant = 0;

		List<Expression> arguments = Plus.getSummands(comparisonSide);
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
						throw new DuplicateTermException(comparisonSide, negationArgument);
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
						throw new DuplicateTermException(comparisonSide, argument);
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
	
	/** 
	 * Adds this {@link DifferenceArithmeticLiteralSide} to another,
	 * or throws an exception if invalid.
	 */
	public DifferenceArithmeticLiteralSide add(DifferenceArithmeticLiteralSide another)
			throws DifferenceArithmeticLiteralSideException {
		
		LinkedHashSet<Expression> resultPositives = new LinkedHashSet<>(getPositives());
		LinkedHashSet<Expression> resultNegatives = new LinkedHashSet<>(getNegatives());
		
		resultPositives.addAll(another.getPositives());
		boolean thereAreTooManyPositives = resultPositives.size() > 1;
		if (thereAreTooManyPositives) {
			throw new InvalidLiteralException("Too many positive variables in " + this + " and " + another);
		}
		boolean thereIsARepeatedPositive = resultPositives.size() < getPositives().size() + another.getPositives().size();
		if (thereIsARepeatedPositive) {
			throw new DuplicateTermException("Duplicate term in " + this + " and " + another);
		}
		
		resultNegatives.addAll(another.getNegatives());
		boolean thereAreTooManyNegatives = resultNegatives.size() > 1;
		if (thereAreTooManyNegatives) {
			throw new InvalidLiteralException("Too many negative variables in " + this + " and " + another);
		}
		boolean thereIsARepeatedNegative = resultNegatives.size() < getNegatives().size() + another.getNegatives().size();
		if (thereIsARepeatedNegative) {
			throw new DuplicateTermException("Duplicate term in " + this + " and " + another);
		}
		
		int resultConstant = this.getConstant() + another.getConstant();
		
		DifferenceArithmeticLiteralSide result =
				new DifferenceArithmeticLiteralSide(resultPositives, resultNegatives, resultConstant);
		
		return result;
	}
	
	/**
	 * Returns the negative of this {@link DifferenceArithmeticLiteralSide}.
	 * @return
	 */
	public DifferenceArithmeticLiteralSide negate() {
		DifferenceArithmeticLiteralSide result =
				new DifferenceArithmeticLiteralSide(getNegatives(), getPositives(), -getConstant());
		return result;
	}
	
	/** 
	 * Subtract another {@link DifferenceArithmeticLiteralSide} from this one,
	 * or throws an exception if invalid.
	 */
	public DifferenceArithmeticLiteralSide subtract(DifferenceArithmeticLiteralSide another)
			throws DifferenceArithmeticLiteralSideException {
		
		DifferenceArithmeticLiteralSide result = this.add(another.negate());
		
		return result;
	}
	
	/**
	 * Given a numerical comparison, return the non-zero side of an equivalent difference arithmetic literal with the other size equal to zero.
	 * @param numericalComparison a numerical comparison expression
	 * @return a {@link DifferenceArithmeticLiteralSide} representing the non-zero side
	 * of an literal equivalent to numericalComparison that has the other side equal to zero.
	 * @throws DuplicateTermException thrown when a term appears with the same sign on the same side of a literal, meaning that the comparison cannot be a difference arithmetic literal.
	 * @throws InvalidLiteralException thrown when the numerical comparison cannot be converted to a difference arithmetic literal.
	 */
	public static DifferenceArithmeticLiteralSide makeDifferenceArithmeticNonZeroSideOfLiteralEquivalentTo(Expression numericalComparison) 
	throws DuplicateTermException, InvalidLiteralException
	{
		DifferenceArithmeticLiteralSide leftHandSide = new DifferenceArithmeticLiteralSide(numericalComparison.get(0)); 
		DifferenceArithmeticLiteralSide rightHandSide = new DifferenceArithmeticLiteralSide(numericalComparison.get(1)); 

		// cancel out terms that are positive in both first and second sides (they cancel because second parts is being subtracted):
		Iterator<Expression> leftPositivesIterator = leftHandSide.positives.iterator();
		while (leftPositivesIterator.hasNext()) {
			Expression leftPositive = leftPositivesIterator.next();
			if (rightHandSide.positives.contains(leftPositive)) {
				leftPositivesIterator.remove();
				rightHandSide.positives.remove(leftPositive);
			}
		}
		
		// cancel out terms that are negative in both first and second parts (they cancel because second parts is being subtracted):
		Iterator<Expression> leftNegativesIterator = leftHandSide.negatives.iterator();
		while (leftNegativesIterator.hasNext()) {
			Expression leftNegative = leftNegativesIterator.next();
			if (rightHandSide.negatives.contains(leftNegative)) {
				leftNegativesIterator.remove();
				rightHandSide.negatives.remove(leftNegative);
			}
		}
		
		Set<Expression> unionOfPositiveTerms = new LinkedHashSet<>();
		Iterable<Expression> positiveTerms = in(new NestedIterator<Expression>(
				leftHandSide.positives,
				rightHandSide.negatives)); // negative terms in second tuple are actually positive since it is being subtracted
		
		for (Expression positive : positiveTerms) {
			boolean noDuplicate = unionOfPositiveTerms.add(positive);
			boolean duplicate = ! noDuplicate;
			if (duplicate) {
				throw new DuplicateTermException(numericalComparison, positive);
			}
			else if (unionOfPositiveTerms.size() == 2) {
				throw new InvalidLiteralException(numericalComparison + " is not a difference arithmetic atom because it contains more than one positive term when moved to the left-hand side: " + join(unionOfPositiveTerms));
			}
		}
		
		Set<Expression> unionOfNegativeTerms = new LinkedHashSet<>();
		Iterable<Expression> negativeTerms = 
				in(
						new NestedIterator<Expression>(
								leftHandSide.negatives,
								rightHandSide.positives)); // positive terms in right-hand tuple are actually negative since it is being subtracted
		
		for (Expression negative : negativeTerms) {
			boolean noDuplicate = unionOfNegativeTerms.add(negative);
			boolean duplicate = ! noDuplicate;
			if (duplicate) {
				throw new DuplicateTermException(numericalComparison, negative);
			}
			else if (unionOfNegativeTerms.size() == 2) {
				throw new InvalidLiteralException(numericalComparison + " is not a difference arithmetic atom because it contains more than one negative term when moved to the left-hand side: " + join(unionOfNegativeTerms));
			}
		}
		
		int constant = leftHandSide.constant - rightHandSide.constant;
	
		DifferenceArithmeticLiteralSide result = new DifferenceArithmeticLiteralSide(unionOfPositiveTerms, unionOfNegativeTerms, constant);
		return result;
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

	public static class DifferenceArithmeticLiteralSideException extends Exception {
		private static final long serialVersionUID = 1L;

		public DifferenceArithmeticLiteralSideException(String message) {
			super(message);
		}
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
	public static class DuplicateTermException extends DifferenceArithmeticLiteralSideException {
		private static final long serialVersionUID = 1L;

		public DuplicateTermException(String message) {
			super(message);
		}
		
		public DuplicateTermException(Expression numericalComparison, Expression duplicate) {
			super(numericalComparison + " is not a difference arithmetic atom because " 
					+ duplicate + " sums with itself, but no multiples are allowed in difference arithmetic");
		}
	}
	
	/**
	 * An error indicating that an expression is not a different arithmetic literal.
	 * @param message
	 * @author braz
	 *
	 */
	public static class InvalidLiteralException extends DifferenceArithmeticLiteralSideException {
		private static final long serialVersionUID = 1L;

		public InvalidLiteralException(String message) {
			super(message);
		}
	}

	@Override
	protected Expression computeInnerExpression() {
		Expression result;
		Expression variablesDifference = 
				BinaryMinus.make(
						Plus.make(new LinkedList<>(positives)),
						Plus.make(new LinkedList<>(negatives))
						);
		if (constant >= 0) {
			result = Plus.make(variablesDifference, Expressions.makeSymbol(constant));
		}
		else {
			result = BinaryMinus.make(variablesDifference, Expressions.makeSymbol(-constant));
		}
		return result;
	}
}