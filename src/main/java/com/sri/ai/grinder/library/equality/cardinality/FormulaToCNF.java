package com.sri.ai.grinder.library.equality.cardinality;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;

public class FormulaToCNF {

	/**
	 * Convert a formula into an inferentially equivalent Conjunctive Normal
	 * Form Expression. Transformation rules are based on the INSEADO method
	 * outlined in:
	 * 
	 * <a href=
	 * "http://logic.stanford.edu/classes/cs157/2012/lectures/lecture09.pdf"
	 * >INSEADO Rules (slide 6 to 9</a>
	 * 
	 * 
	 * @param formula
	 *            a formula.
	 * @param process
	 *            the rewriting process
	 * @return false, true, a clause, or a conjunction of clauses.
	 * @throws IllegalArgumentException
	 *             if the input formula expression is not actually a formula.
	 * @see CardinalityUtil#isFormula(Expression, RewritingProcess)
	 */
	public static Expression convertToCNF(Expression formula,
			RewritingProcess process) {
		Expression result = formula;

		if (!CardinalityUtil.isFormula(formula, process)) {
			throw new IllegalArgumentException(
					"Expression to be converted is not a formula: " + formula);
		}
					
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
					new NormalizeOrRewriter(),
					new NormalizeAndRewriter(),
					new NormalizeEqualitiesRewriter()
				));
			
		result = cnfRewriter.rewrite(formula, process);			
		if (!(result.equals(Expressions.TRUE) || result.equals(Expressions.FALSE))) {
			if (!isInCNF(result)) {
				throw new IllegalStateException("Failed to convert to CNF: "+result);
			}
		}

		return result;
	}

	/**
	 * Determine if the given expression is in CNF form.
	 * 
	 * @param expression
	 *            the expression to be tested if in CNF form.
	 * @return true if is in CNF form, false otherwise.
	 */
	public static boolean isInCNF(Expression expression) {
		boolean result = false;

		if (And.isConjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each conjunct must be a clause.
			for (Expression conjunct : expression.getArguments()) {
				if (!isCNFClause(conjunct)) {
					result = false;
					break;
				}
			}

		} else {
			result = isCNFClause(expression);
		}

		return result;
	}

	/**
	 * Determine if the given expression is a CNF Clause (i.e. a disjunction of
	 * literals only).
	 * 
	 * @param expression
	 *            the expression to be tested if a CNF clause.
	 * @return true if a CNF clause, false otherwise.
	 */
	public static boolean isCNFClause(Expression expression) {
		boolean result = false;
		
		if (Or.isDisjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each disjunct must be a literal (no nesting allowed).
			for (Expression disjunct : expression.getArguments()) {
				if (!isCNFLiteral(disjunct)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}

	/**
	 * Determine if the given expression is a CNF literal, i.e. an equality or
	 * disequality with 2 arguments, i.e. we exclude 'X = Y = Z' types of
	 * equality expressions.
	 * 
	 * @param expression
	 *            the expression to be tested whether or not is a 2 argument
	 *            equality or inequality.
	 * @return true if is a 2 argument equality or inequality.
	 */
	public static boolean isCNFLiteral(Expression expression) {
		boolean result = false;

		if (Equality.isEquality(expression)
				|| Disequality.isDisequality(expression)) {
			if (expression.numberOfArguments() == 2) {
				result = true;
			}
		}

		return result;
	}

	//
	// PRIVATE
	//
	/**
	 * Performs the following normalizations on the formula:
	 * 
	 * or()                -> false
	 * or(..., true, ...)  -> true
	 * or(..., false, ...) -> or(..., ...)
	 * 
	 */
	private static class NormalizeOrRewriter extends AbstractRewriter {
			
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (Or.isDisjunction(expression)) {
				// or() -> false
				if (expression.numberOfArguments() == 0) {
					result = Expressions.FALSE;
				}
				else {
					// or(..., true, ...)  -> true
					// or(..., false, ...) -> or(..., ...)
					List<Expression> literals = new ArrayList<Expression>();
					for (Expression disjunct : expression.getArguments()) {
						if (disjunct.equals(Expressions.TRUE)) {
							result = Expressions.TRUE;
							break;
						}
						else {
							if (!disjunct.equals(Expressions.FALSE)) {
								literals.add(disjunct);
							}
						}
					}
					if (!result.equals(Expressions.TRUE) && 
						literals.size() < expression.numberOfArguments()) {
						result = Or.make(literals);
					}
				}
			}
			
			return result;
		}
	}

	/**
	 * Performs the following normalizations on the formula:
	 * and()                -> true
	 * and(..., true, ...)  -> and(..., ...)
	 * and(..., false, ...) -> false
	 */
	private static class NormalizeAndRewriter extends AbstractRewriter {
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (And.isConjunction(expression)) {
				// and() -> true
				if (expression.numberOfArguments() == 0) {
					result = Expressions.TRUE;
				}
				else {
					// and(..., true, ...)  -> and(..., ...)
					// and(..., false, ...) -> false
					List<Expression> literals = new ArrayList<Expression>();
					for (Expression conjunct : expression.getArguments()) {
						if (conjunct.equals(Expressions.FALSE)) {
							result = Expressions.FALSE;
							break;
						}
						else {
							if (!conjunct.equals(Expressions.TRUE)) {
								literals.add(conjunct);
							}
						}
					}
					if (!result.equals(Expressions.FALSE) && 
						literals.size() < expression.numberOfArguments()) {
						result = And.make(literals);
					}
				}
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the following normalizations on equalities:
	 * 
	 * X = Y = Z = -> X = Y and Y = Z
	 * a = X       -> X = a
	 * a != X      -> X != a
	 * X = X       -> true
	 * X != X      -> false
	 * a = b       -> false
	 * a != b      -> true
	 */
	private static class NormalizeEqualitiesRewriter extends AbstractRewriter {
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;			
			if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
				// X = Y = Z -> X = Y and Y = Z
				if (Equality.isEquality(expression) && expression.numberOfArguments() > 2) {
					List<Expression> conjuncts = new ArrayList<Expression>();
					for (int i = 0; i < expression.numberOfArguments()-1; i++) {
						conjuncts.add(Equality.make(expression.get(i), expression.get(i+1)));
					}
					result = And.make(conjuncts);
				}
				else {
					// a = X  -> X = a
					// a != X -> X != a
					Expression normalized = Equality.normalize(expression, process);
					if (normalized != expression) {
						result = normalized;
					}
					else {
						// X = X  -> true
						// X != X -> false
						if (expression.get(0).equals(expression.get(1))) {
							if (Equality.isEquality(expression)) {
								result = Expressions.TRUE;
							}
							else {
								result = Expressions.FALSE;
							}
						}
						else {
							// a = b  -> false
							// a != b -> true
							if (process.isConstant(expression.get(0)) && process.isConstant(expression.get(1))) {
								if (Equality.isEquality(expression)) {
									result = Expressions.FALSE;
								}
								else {
									result = Expressions.TRUE;
								}
							}
						}
					}
				}
			}
			return result;
		}
	}
}
